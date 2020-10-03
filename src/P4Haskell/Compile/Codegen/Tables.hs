-- |
module P4Haskell.Compile.Codegen.Tables
  ( generateTableCall,
  )
where

import Data.Bit
import Data.Foldable
import Data.Generics.Sum.Typed
import qualified Data.HashMap.Lazy as LH
import qualified Data.Vector.Unboxed as V
import qualified Language.C99.Pretty as PC
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Codegen.Utils
import P4Haskell.Compile.Declared
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer
import Relude (error)
import Relude.Extra (toPairs)
import Relude.Unsafe (fromJust, (!!))
import qualified Text.PrettyPrint as TP

filterMapVec :: (v -> Bool) -> AST.MapVec k v -> AST.MapVec k v
filterMapVec f (AST.MapVec m v) = AST.MapVec (LH.filter f m) (filter f v)

-- TODO: Have params for passing table configurations
-- TODO: Handle each type of table key
-- TODO: decide on how to implement LPM

data TableMatchKind
  = Exact
  | Ternary
  | LPM
  deriving (Generic, Show)

generateTableKeys :: (CompC r, Member (Writer [C.BlockItem]) r) => [AST.KeyElement] -> Sem r [(C.Expr, C.TypeSpec, TableMatchKind, Int)]
generateTableKeys elems =
  mapM
    ( \e -> do
        matchKinds <- fetch GetTopLevelMatchKind
        let kind = fromJust $ matchKinds ^. at (e ^. #matchType ^. #name)
        let matchKind = case kind ^. #name of
              "exact" -> Exact
              "ternary" -> Ternary
              "lpm" -> LPM
              k -> error $ "Unsupported match kind: " <> k
        tmpName <- generateTempVar
        let p4ty = gdrillField @"type_" $ e ^. #expression
        (_, ty) <- generateP4Type p4ty
        expr <- generateP4Expression $ e ^. #expression
        tell [C.Decln $ C.VarDecln Nothing (C.TypeSpec ty) tmpName (Just . C.InitExpr $ expr)]
        pure (C.Ident tmpName, ty, matchKind, typeSize p4ty)
    )
    elems

matchTreeNodeType :: C.TypeSpec
matchTreeNodeType =
  C.StructDecln
    (Just "match_tree_node")
    ( fromList
        [ C.FieldDecln (C.TypeSpec $ C.TypedefName "uint16_t") "param_idx",
          C.FieldDecln (C.TypeSpec $ C.TypedefName "uint16_t") "action_idx",
          C.FieldDecln (C.Array (C.TypeSpec $ C.TypedefName "int16_t") (Just . C.LitInt $ 2 ^ bitsPerLevel @Int)) "offsets"
        ]
    )

makeNode :: Integer -> Integer -> [Integer] -> C.Expr
makeNode actionIdx paramIdx offsets =
  C.InitVal
    (C.TypeName . C.TypeSpec . C.Struct $ "match_tree_node")
    ( fromList
        [ C.InitItem (Just "param_idx") (C.InitExpr $ C.LitInt paramIdx),
          C.InitItem (Just "action_idx") (C.InitExpr $ C.LitInt actionIdx),
          C.InitItem
            (Just "offsets")
            ( C.InitMultiple . fromList . reverse $
                map (C.InitItem Nothing . C.InitExpr . C.LitInt) offsets
            )
        ]
    )

-- TODO(optimisation): decide on the bits per level dynamically
bitsPerLevel :: Num a => a
bitsPerLevel = 4

data BitChunk
  = BitChunk Int
  | AcceptAll
  deriving (Show, Generic)

typeSize :: AST.P4Type -> Int
typeSize p4ty = case p4ty of
  AST.TypeBits'P4Type (AST.TypeBits l _) -> l
  _ -> error $ "The type: " <> show p4ty <> " is unsupported in table keys"

chunkVec :: V.Unbox a => Int -> V.Vector a -> [V.Vector a]
chunkVec interval v = [V.slice i interval v | i <- [0, interval .. (V.length v - interval)]]

cielDiv :: Integral a => a -> a -> a
cielDiv a b = (a + b - 1) `div` b

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

generateBitChunks :: [(Int, TableMatchKind)] -> AST.TableEntry -> [BitChunk]
generateBitChunks meta (AST.TableEntry keys _ _) = concatMap (uncurry inner) (zip keys meta)
  where
    inner (AST.Constant'SelectKey (AST.Constant _ v _)) (width, _matchKind) =
      let vec = fromIntegral @_ @Word v & V.singleton & castFromWords & V.reverse
          chunks =
            map (V.head . cloneToWords8 . V.reverse)
              . lastN (cielDiv width bitsPerLevel)
              $ chunkVec bitsPerLevel vec
       in map (BitChunk . fromIntegral) chunks
    inner (AST.Default'SelectKey (AST.DefaultExpression _)) (width, _matchKind) =
      replicate (cielDiv width bitsPerLevel) AcceptAll

data ChunkTrie
  = ChunkTrieLeaf (Int, Int)
  | ChunkTrieNode [Maybe Int]
  deriving (Show, Generic)

-- isLeaf :: ChunkTrie -> Bool
-- isLeaf (ChunkTrieLeaf _) = True
-- isLeaf _ = False

isNode :: ChunkTrie -> Bool
isNode (ChunkTrieNode _) = True
isNode _ = False

type TrieBuildC r = Members [State (HashMap Int ChunkTrie), State Int] r

nodeToExpr :: Int -> ChunkTrie -> C.Expr
nodeToExpr myId (ChunkTrieNode ns) =
  let offsets = map toInteger $ map (maybe (- myId) (subtract myId)) ns
   in makeNode 0 0 offsets
nodeToExpr _ (ChunkTrieLeaf (aid, pid)) =
  let offsets = replicate (2 ^ bitsPerLevel @Int) 0
   in makeNode (toInteger aid) (toInteger pid) offsets

trieToExpr :: [(Int, ChunkTrie)] -> [C.Expr]
trieToExpr t =
  let nodes = map (\(nid, n) -> (nid, nodeToExpr nid n)) t
      deadNode = (0, makeNode 0 0 $ replicate (2 ^ bitsPerLevel @Int) 0)
   in -- sort in reverse since the C dsl orders arrays in reverse
      map snd $ sortOn (Down . fst) (deadNode : nodes)

emptyTrieNode :: [Maybe Int]
emptyTrieNode = replicate (2 ^ bitsPerLevel @Int) Nothing

getNodeId :: Member (State Int) r => Sem r Int
getNodeId = get <* modify (+ 1)

nameNode :: TrieBuildC r => ChunkTrie -> Sem r (Int, ChunkTrie)
nameNode n = do
  nid <- getNodeId
  modify @(HashMap Int ChunkTrie) . (<>) $ fromList [(nid, n)]
  pure (nid, n)

makeTrieTail :: TrieBuildC r => [BitChunk] -> (Int, Int) -> Sem r ChunkTrie
makeTrieTail cs v = foldrM go (ChunkTrieLeaf v) cs
  where
    go (BitChunk bp) n =
      nameNode n
        <&> \(nid, _) -> ChunkTrieNode (emptyTrieNode & ix bp ?~ nid)
    go AcceptAll n =
      nameNode n
        <&> \(nid, _) -> ChunkTrieNode (emptyTrieNode $> Just nid)

isEmptyAt :: Int -> [Maybe a] -> Bool
isEmptyAt n l = isNothing (l !! n)

addEntry :: TrieBuildC r => [BitChunk] -> (Int, Int) -> ChunkTrie -> Sem r ChunkTrie
addEntry (BitChunk bp : cs) v (ChunkTrieNode xs@(isEmptyAt bp -> True)) = do
  (tid, _) <- nameNode =<< makeTrieTail cs v
  pure $ ChunkTrieNode (xs & ix bp ?~ tid)
addEntry (BitChunk bp : cs) v (ChunkTrieNode xs@((!! bp) -> Just nid)) = do
  n <- gets @(HashMap Int ChunkTrie) (^?! ix nid)
  n' <- addEntry cs v n
  modify @(HashMap Int ChunkTrie) (at nid ?~ n')
  pure $ ChunkTrieNode xs
addEntry (AcceptAll : cs) v (ChunkTrieNode xs) = do
  (tid, _) <- nameNode =<< makeTrieTail cs v
  xs' <- mapM ((Just <$>) . maybe (pure tid) insertOverlapping) xs
  pure $ ChunkTrieNode xs'
  where
    insertOverlapping :: TrieBuildC r => Int -> Sem r Int
    insertOverlapping nid = do
      n <- gets @(HashMap Int ChunkTrie) (^?! ix nid)
      when (isNode n) do
        n' <- addEntry cs v n
        modify @(HashMap Int ChunkTrie) (at nid ?~ n')
      pure nid
addEntry bp v n = error $ "failed to insert entry into search trie: bp=" <> show bp <> ", v=" <> show v <> ", n=" <> show n

-- logState :: (Show s, Member (State s) r) => Sem r a -> Sem r a
-- logState m = get >>= \s -> trace (toString $ pShow s) m

buildTrie :: [([BitChunk], (Int, Int))] -> (HashMap Int ChunkTrie, Int)
buildTrie inp = run . runState @(HashMap Int ChunkTrie) mempty . evalState @Int 1 $ do
  let root = ChunkTrieNode emptyTrieNode
  (rid, _) <- nameNode =<< foldlM (flip $ uncurry addEntry) root inp
  pure rid

generateTableTrie :: CompC r => Text -> HashMap Text ProcessedAction -> [AST.TableEntry] -> [(Int, TableMatchKind)] -> Sem r (C.Expr, Int)
generateTableTrie tableName pactions entries meta =
  let chunks = map (generateBitChunks meta) entries
      paramIds = [0 ..]
      actionIds = map (\e -> pactions ^?! ix (e ^?! #action . #method . _Typed @AST.PathExpression . #path . #name) . #id) entries
      (trieNodes, rootId) = buildTrie $ zip chunks $ zip actionIds paramIds
      trieInits = trieToExpr $ toPairs trieNodes
      trieInit = C.InitMultiple . fromList . map (C.InitItem Nothing . C.InitExpr) $ trieInits
      staticName = tableName <> "_search_trie"
   in do
        treeNodeTy <- simplifyType matchTreeNodeType
        let arrayTy = C.Array (C.TypeSpec treeNodeTy) (Just . C.LitInt . fromIntegral $ length trieInits)
        modify . (<>) $ defineStatic staticName Nothing arrayTy trieInit
        pure (C.Ident . toString $ staticName, rootId)

makeCase :: ([C.BlockItem], Integer) -> C.Case
makeCase (stmts, caseId) =
  C.Case
    (C.LitInt caseId)
    (C.Block $ stmts <> [C.Stmt C.Break])

selectChunk :: C.Expr -> Integer -> C.Expr -> C.Expr
selectChunk e totalWidth n =
  (e C..>> (C.LitInt totalWidth C..- (n C..* C.LitInt bitsPerLevel)))
    C..& ((C.LitInt 1 C..<< C.LitInt bitsPerLevel) C..- C.LitInt 1)

generateBitDriverFor :: CompC r => C.TypeSpec -> Int -> Sem r C.Expr
generateBitDriverFor ty width = do
  treeNodeTy <- simplifyType matchTreeNodeType
  let params =
        [ C.Param (C.Ptr . C.TypeSpec $ treeNodeTy) "node",
          C.Param (C.TypeSpec ty) "value"
        ]
  modify . (<>) $
    defineFunc name (C.Ptr . C.TypeSpec $ treeNodeTy) params body
  pure . C.Ident $ toString name
  where
    chunks = cielDiv width bitsPerLevel
    totalBits = chunks * bitsPerLevel
    idx = C.Ident "idx"
    node = C.Ident "node"
    name = "table_trie_driver_w" <> show chunks
    body =
      [ C.Decln $ C.VarDecln Nothing (C.TypeSpec $ C.TypedefName "size_t") "idx" Nothing,
        C.Stmt $
          C.For
            (idx C..= (C.LitInt 0))
            (idx C..< (C.LitInt $ fromIntegral chunks))
            ((C..++) idx)
            [ C.Stmt $ C.Expr (node C..+= C.Index (C.Arrow node "offsets") (selectChunk (C.Ident "value") (fromIntegral totalBits) idx))
            ],
        C.Stmt $ C.Return (Just node)
      ]

-- generate action options: the action name and required parameters
-- generate structs and union with the required parameters

data ProcessedAction = ProcessedAction
  { id :: Int,
    nExtraParams :: Int,
    variantName :: Text,
    paramCtor :: [C.Expr] -> C.Expr,
    actionCode :: [C.BlockItem]
  }
  deriving (Generic)

fixEmptyFields :: [C.FieldDecln] -> NonEmpty C.FieldDecln
fixEmptyFields = fromMaybe (C.FieldDecln (C.TypeSpec C.Char) "unused" :| []) . nonEmpty

structForRuntimeParams :: CompC r => Text -> [AST.Parameter] -> Sem r C.TypeSpec
structForRuntimeParams name params = do
  types <-
    mapM
      ( \p -> do
          (ty, _) <- generateP4Type $ p ^. #type_
          pure (C.TypeSpec ty, p ^. #name)
      )
      params
  pure . C.StructDecln (Just . toString $ name) . fixEmptyFields . map (\(t, n) -> C.FieldDecln t $ toString n) $ types

generateActions :: CompC r => AST.P4Table -> C.Expr -> C.Expr -> Sem r (HashMap Text ProcessedAction, C.TypeSpec)
generateActions t argsTable argsIndex = do
  -- bit of a hack since we generate this later
  let unionName = (t ^. #name) <> "_param_union"

  (actions, variants) <-
    unzip
      <$> mapM
        ( \(a, i) -> do
            let name = a ^?! #expression . #method . _Typed @AST.PathExpression . #path . #name
            let args = a ^. #expression . #arguments
            action' <- fromJust <$> (Polysemy.Reader.asks $ findActionInScope name)
            let nExtraParams = length (action' ^. #parameters . #vec) - length args
            let runtimeParams = drop (length args) (action' ^. #parameters . #vec)
            let runtimeParamNames = runtimeParams ^.. traverse . #name
            let variantName = "arg_table_" <> (t ^. #name) <> "_" <> name
            ty <- simplifyType =<< structForRuntimeParams variantName runtimeParams
            let ctor params =
                  C.InitVal (C.TypeName . C.TypeSpec $ ty)
                    . fromMaybe (C.InitItem (Just "unused") (C.InitExpr $ C.LitInt 0) :| [])
                    . nonEmpty
                    . map (\(e, n) -> C.InitItem (Just $ toString n) (C.InitExpr e))
                    $ zip params runtimeParamNames

            -- a bit spooky, create temp_vars and use them for the extra params
            extraVars <- replicateM nExtraParams generateTempVar

            let extraVarExprs =
                  map
                    ( \(p, temp) ->
                        AST.Argument (Just $ p ^. #name)
                          . AST.PathExpression'Expression
                          . AST.PathExpression (p ^. #type_)
                          $ AST.Path False (toText temp)
                    )
                    $ zip runtimeParams extraVars

            let patchedExpr = (a ^. #expression) & #arguments <>~ extraVarExprs

            extraVars' <-
              mapM
                ( \(p, temp) -> do
                    (ty, _) <- generateP4Type $ p ^. #type_
                    makeVar (toText temp) (C.TypeSpec ty) (p ^. #type_) False
                )
                $ zip runtimeParams extraVars

            let updatedScope scope = foldl (flip addVarToScope) scope extraVars'

            extraVarDecls <-
              mapM
                ( \(p, temp) -> do
                    (ty, _) <- generateP4Type $ p ^. #type_
                    pure . C.Decln $
                      C.VarDecln
                        Nothing
                        (C.TypeSpec ty)
                        temp
                        ( Just . C.InitExpr $
                            (C.Index argsTable argsIndex)
                              `C.Dot` (toString variantName)
                              `C.Dot` (toString $ p ^. #name)
                        )
                )
                $ zip runtimeParams extraVars

            (stmts, expr) <-
              runWriterAssocR
                . local updatedScope
                . generateP4Expression
                $ AST.MethodCallExpression'Expression patchedExpr

            let stmts' = extraVarDecls <> stmts <> [C.Stmt . C.Expr $ expr]

            pure ((name, ProcessedAction i nExtraParams variantName ctor stmts'), (variantName, ty))
        )
        (zip (t ^. #actions . #actions . #vec) [1 ..])

  unionTy <-
    simplifyType $
      C.UnionDecln (Just . toString $ unionName)
        . fixEmptyFields
        . map (\(n, t) -> C.FieldDecln (C.TypeSpec t) $ toString n)
        $ variants
  pure (fromList actions, unionTy)

evalConstantLit :: AST.Expression -> C.Expr
evalConstantLit (AST.Constant'Expression c) = C.LitInt . fromIntegral $ c ^. #value
evalConstantLit e = error $ show e <> " was not a constant expression"

generateParamTable :: HashMap Text ProcessedAction -> C.TypeSpec -> [AST.MethodCallExpression] -> [C.Expr]
generateParamTable actions ty exprs =
  map
    ( \e ->
        let name = e ^?! #method . _Typed @AST.PathExpression . #path . #name
            act = actions ^?! ix name
            params = drop (length (e ^. #arguments) - act ^. #nExtraParams) (e ^. #arguments)
            paramExprs :: [C.Expr] = map (evalConstantLit . (^. #expression)) params
            ctor = (act ^. #paramCtor) paramExprs
         in C.InitVal (C.TypeName $ C.TypeSpec ty) (fromList [C.InitItem (Just . toString $ act ^. #variantName) (C.InitExpr ctor)])
    )
    exprs

generateTableCall :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.TypeTable -> AST.TypeStruct -> Sem r C.Expr
generateTableCall (AST.TypeTable table) rty = do
  let rty' = rty & #fields %~ filterMapVec ((/= "action_run") . (^. #name))
  (rty'', _) <- generateP4Type $ AST.TypeStruct'P4Type rty'
  keys <- generateTableKeys $ table ^. #keys

  let entries = fromMaybe (error "tables without entries are not yet supported") $ table ^. #entries

  nodeVarName <- generateTempVar
  let nodeVar = C.Ident nodeVarName

  let argsTableName = "arg_table_" <> (table ^. #name) <> "_entries"
  (processedActions, paramUnion) <- generateActions table (C.Ident . toString $ argsTableName) (C.Arrow nodeVar "param_idx")

  let paramTable = generateParamTable processedActions paramUnion (entries ^.. traverse . #action)
  modify . (<>) $
    defineStatic
      argsTableName
      Nothing
      (C.Array (C.TypeSpec paramUnion) Nothing)
      (C.InitMultiple . fromList $ map (C.InitItem Nothing . C.InitExpr) (reverse paramTable))

  (searchTrie, rootNode) <- generateTableTrie (table ^. #name) processedActions entries (map (\(_, _, a, b) -> (b, a)) keys)

  treeNodeTy <- simplifyType matchTreeNodeType
  let nodePtrTy = C.Ptr . C.TypeSpec $ treeNodeTy

  tell
    [ C.Decln $
        C.VarDecln
          Nothing
          nodePtrTy
          nodeVarName
          (Just . C.InitExpr $ C.Index searchTrie (C.LitInt $ fromIntegral rootNode))
    ]

  forM_
    keys
    ( \(e, ty, _, w) -> do
        fn <- generateBitDriverFor ty w
        tell [C.Stmt . C.Expr $ nodeVar C..= C.Funcall fn [nodeVar, e]]
    )

  let cases = [C.Default C.Break] <> map makeCase (zip (processedActions ^.. traverse . #actionCode) [1 ..])

  tell [C.Stmt $ C.Switch (C.Arrow nodeVar "action_idx") cases]

  pure $ C.LitInt 0
