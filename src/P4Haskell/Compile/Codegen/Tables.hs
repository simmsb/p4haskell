-- |
module P4Haskell.Compile.Codegen.Tables
  ( generateTableCall,
  )
where

import Data.Foldable
import Data.Generics.Sum
import qualified Data.HashMap.Lazy as LH
import Data.Text.Lens (unpacked)
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Expression
import P4Haskell.Compile.Codegen.Extern
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import P4Haskell.Compile.Codegen.Statement
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
import Relude.Unsafe ((!!), fromJust)
import Data.Bit
import qualified Data.Vector.Unboxed as V
import Relude.Extra (toPairs)

filterMapVec :: (v -> Bool) -> AST.MapVec k v -> AST.MapVec k v
filterMapVec f (AST.MapVec m v) = AST.MapVec (LH.filter f m) (filter f v)

-- TODO: Have params for passing table configurations
-- TODO: Handle each type of table key

data LiftedAction = LiftedAction
  { nameExpr :: C.Expr,
    liftedParams :: [AST.Parameter],
    liftedParamExprs :: [AST.Expression],
    originalParams :: [AST.Parameter]
  }
  deriving (Generic)

interceptUnknownVars :: CompC r => Sem r a -> Sem r (Scope, a)
interceptUnknownVars m = do
  parentScope <- Polysemy.Reader.ask
  runState emptyScope . intercept @ScopeLookup (inner parentScope) . raise . local (const emptyScope) $ m
  where
    inner :: (CompC r, Member (State Scope) r) => forall m x. Scope -> ScopeLookup m x -> Sem r x
    inner parentScope (LookupVarInScope name p4ty) = do
      val <- Polysemy.Reader.asks $ findVarInScope name
      case val of
        Just v -> pure (Just v)
        Nothing -> do
          val' <- Polysemy.State.gets $ findVarInScope name
          case val' of
            Just v -> pure (Just v)
            Nothing -> do
              case findVarInScope name parentScope of
                Just _ -> do
                  (ty, _) <- generateP4Type p4ty
                  var <- makeVar name (C.TypeSpec ty) p4ty True
                  modify $ addVarToScope var
                  pure (Just var)
                Nothing -> pure Nothing

generateActionParams :: CompC r => AST.P4Action -> Sem r ([C.Param], [Var])
generateActionParams a =
  unzip
    <$> forM
      (a ^. #parameters . #vec)
      ( \param -> do
          (ty, _) <- generateP4Type (param ^. #type_)
          let isOut = param ^. #direction . #out
          var <- makeVar (param ^. #name) (C.TypeSpec ty) (param ^. #type_) isOut
          let ty' = if isOut then C.Ptr (C.TypeSpec ty) else (C.TypeSpec ty)
          pure (C.Param ty' (toString $ param ^. #name), var)
      )

liftAction :: CompC r => AST.P4Action -> Sem r LiftedAction
liftAction action = do
  (params, vars) <- generateActionParams action
  let scopeUpdate scope = flipfoldl' addVarToScope scope vars
  (extraVars, body) <-
    local scopeUpdate . interceptUnknownVars . generateStatements $
      action ^. #body . #components
  let body' = removeDeadExprs body
  let (extraCParams, extraP4Params, paramExprs) =
        unzip3 $ map
          (\v -> ( C.Param (v ^. #varType) (toString $ v ^. #varOriginalName)
                 , AST.Parameter [] (AST.Direction True True) (v ^. #varOriginalName) (v ^. #varP4Type)
                 , AST.PathExpression'Expression $ AST.PathExpression (v ^. #varP4Type) (AST.Path False $ v ^. #varOriginalName)
                 ))
          (LH.elems $ extraVars ^. #scopeVarBindings)
  modify . (<>) $ defineFunc (action ^. #name) (C.TypeSpec C.Void) (params <> extraCParams) body'
  pure $ LiftedAction (C.Ident . toString $ action ^. #name) extraP4Params paramExprs (action ^. #parameters . #vec)

data TableMatchKind
  = Exact
  | Ternary
  | LPM
  deriving ( Generic, Show )

generateTableKeys :: (CompC r, Member (Writer [C.BlockItem]) r) => [AST.KeyElement] -> Sem r [(C.Expr, TableMatchKind, Int)]
generateTableKeys elems =
  mapM
    ( \e -> do
        matchKinds <- fetch GetTopLevelMatchKind
        let kind = fromJust $ matchKinds ^. at (e ^. #matchType ^. #name)
        let matchKind = case kind ^. #name of
              "exact"   -> Exact
              "ternary" -> Ternary
              "lpm"     -> LPM
              k         -> error $ "Unsupported match kind: " <> k
        tmpName <- generateTempVar
        let p4ty = gdrillField @"type_" $ e ^. #expression
        (_, ty) <- generateP4Type p4ty
        expr <- generateP4Expression $ e ^. #expression
        tell [C.Decln $ C.VarDecln Nothing (C.TypeSpec ty) tmpName (Just . C.InitExpr $ expr)]
        pure (C.Ident tmpName, matchKind, typeSize p4ty)
    )
    elems

matchTreeNodeType :: C.TypeSpec
matchTreeNodeType =
  C.StructDecln
    (Just "match_tree_node")
    ( fromList
        [ C.FieldDecln (C.TypeSpec $ C.TypedefName "uint16_t") "value",
          C.FieldDecln (C.Array (C.TypeSpec $ C.TypedefName "int16_t") (Just . C.LitInt $ 2 ^ bitsPerLevel)) "offsets"
        ]
    )

makeNode :: Integer -> [Integer] -> C.Expr
makeNode val offsets = C.InitVal
  (C.TypeName . C.TypeSpec . C.Struct $ "match_tree_node")
  (fromList [ C.InitItem (Just "value") (C.InitExpr $ C.LitInt val)
            , C.InitItem (Just "offsets") (C.InitMultiple . fromList $
                                           map (C.InitItem Nothing . C.InitExpr . C.LitInt) offsets)
            ])

-- TODO(optimisation): decide on the bits per level dynamically
bitsPerLevel :: Int
bitsPerLevel = 4

data BitChunk
  = BitChunk Int
  | AcceptAll
  deriving ( Show, Generic )

typeSize :: AST.P4Type -> Int
typeSize p4ty = case p4ty of
                  AST.TypeBits'P4Type (AST.TypeBits l _) -> l
                  _ -> error $ "The type: " <> show p4ty <> " is unsupported in table keys"

chunkVec :: V.Unbox a => Int -> V.Vector a -> [V.Vector a]
chunkVec interval v = [V.slice i interval v | i <- [0, interval .. (V.length v - interval)]]

cielDiv :: Int -> Int -> Int
cielDiv a b = (a + b) `div` b

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

generateBitChunks :: [Int] -> AST.TableEntry -> [BitChunk]
generateBitChunks  widths(AST.TableEntry keys _ _) = concatMap (uncurry inner) (zip keys widths)
  where inner (AST.Constant'SelectKey (AST.Constant _ v _)) width =
          let vec = fromIntegral @_ @Word v & V.singleton & castFromWords & V.reverse
              chunks = map (V.head . cloneToWords8 . V.reverse)
                       . lastN (cielDiv width bitsPerLevel)
                       $ chunkVec bitsPerLevel vec
           in map (BitChunk . fromIntegral) chunks
        inner (AST.Default'SelectKey (AST.DefaultExpression _)) width =
          replicate (cielDiv width bitsPerLevel) AcceptAll

data ChunkTrie
  = ChunkTrieLeaf Int
  | ChunkTrieNode [Maybe Int]
  deriving ( Show, Generic )

type TrieBuildC r = Members [State (HashMap Int ChunkTrie), State Int] r

nodeToExpr :: Int -> ChunkTrie -> C.Expr
nodeToExpr myId (ChunkTrieNode ns) =
  let offsets = map toInteger $ map (maybe (-myId) (subtract myId)) ns
   in makeNode 0 offsets
nodeToExpr _ (ChunkTrieLeaf v) =
  let offsets = replicate (2 ^ bitsPerLevel) 0
   in makeNode (toInteger v) offsets

trieToExpr :: [(Int, ChunkTrie)] -> [C.Expr]
trieToExpr t =
  let nodes = map (\(nid, n) -> (nid, nodeToExpr nid n)) t
      deadNode = (0, makeNode 0 $ replicate (2 ^ bitsPerLevel) 0)
      -- sort in reverse since the C dsl orders arrays in reverse
   in map snd $ sortOn (Down . fst) (deadNode : nodes)

emptyTrieNode :: [Maybe Int]
emptyTrieNode = replicate (2 ^ bitsPerLevel) Nothing

getNodeId :: Member (State Int) r => Sem r Int
getNodeId = get <* modify (+ 1)

nameNode :: TrieBuildC r => ChunkTrie -> Sem r (Int, ChunkTrie)
nameNode n = do
  nid <- getNodeId
  modify @(HashMap Int ChunkTrie) . (<>) $ fromList [(nid, n)]
  pure (nid, n)

makeTrieTail :: TrieBuildC r => [BitChunk] -> Int -> Sem r ChunkTrie
makeTrieTail cs v = foldrM go (ChunkTrieLeaf v) cs
  where go (BitChunk bp) n = nameNode n <&>
          \(nid, _) -> ChunkTrieNode (emptyTrieNode & ix bp ?~ nid)
        go AcceptAll n = nameNode n <&>
          \(nid, _) -> ChunkTrieNode (emptyTrieNode $> Just nid)

isEmptyAt :: Int -> [Maybe a] -> Bool
isEmptyAt n l = isNothing (l !! n)

addEntry :: TrieBuildC r => [BitChunk] -> Int -> ChunkTrie -> Sem r ChunkTrie
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
  pure . ChunkTrieNode $ map (Just . fromMaybe tid) xs
addEntry _ _ _ = error "failed to insert entry into search trie"

buildTrie :: [([BitChunk], Int)] -> (HashMap Int ChunkTrie, Int)
buildTrie inp = run . runState @(HashMap Int ChunkTrie) mempty . evalState @Int 1 $ do
  let root = ChunkTrieNode emptyTrieNode
  (rid, _) <- nameNode =<< foldlM (flip $ uncurry addEntry) root inp
  pure rid

generateTableTrie :: CompC r => Text -> [AST.TableEntry] -> [Int] -> Sem r (C.Expr, Int)
generateTableTrie tableName entries widths =
  let chunks = map (generateBitChunks widths) entries
      actionIds = [1..]
      (trieNodes, rootId) = buildTrie $ zip chunks actionIds
      trieInits = trieToExpr $ toPairs trieNodes
      trieInit = C.InitMultiple . fromList . map (C.InitItem Nothing . C.InitExpr) $ trieInits
      staticName = tableName <> "_search_trie"
   in do
    putStrLn $ "chunks: " <> show chunks
    treeNodeTy <- simplifyType matchTreeNodeType
    let arrayTy = C.Array (C.TypeSpec treeNodeTy) (Just . C.LitInt . fromIntegral $ length trieInits)
    modify . (<>) $ defineStatic staticName Nothing arrayTy trieInit
    pure (C.Ident . toString $ staticName, rootId)

generateTableCall :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.TypeTable -> Text -> AST.TypeStruct -> Sem r C.Expr
generateTableCall (AST.TypeTable table) name rty = do
  let rty' = rty & #fields %~ filterMapVec ((/= "action_run") . (^. #name))
  (rty'', _) <- generateP4Type $ AST.TypeStruct'P4Type rty'
  keys <- generateTableKeys $ table ^. #keys

  let entries = fromMaybe (error "tables without entries are not yet supported") $ table ^. #entries
  (searchTrie, rootNode) <- generateTableTrie (table ^. #name) entries (map (^. _3) keys)

  pure $ C.LitInt 1
