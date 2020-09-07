-- |
module P4Haskell.Compile.Codegen.Tables
  ( generateTableCall,
  )
where

import Data.Generics.Sum
import qualified Data.HashMap.Lazy as LH
import Data.Text.Lens (unpacked)
import qualified Generics.SOP as GS
import qualified Language.C99.Simple as C
import P4Haskell.Compile.Codegen.Extern
import {-# SOURCE #-} P4Haskell.Compile.Codegen.MethodCall
import P4Haskell.Compile.Codegen.Typegen
import P4Haskell.Compile.Eff
import P4Haskell.Compile.Fetch
import P4Haskell.Compile.Query
import P4Haskell.Compile.Scope
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Relude (error)

filterMapVec :: (v -> Bool) -> AST.MapVec k v -> AST.MapVec k v
filterMapVec f (AST.MapVec m v) = AST.MapVec (LH.filter f m) (filter f v)

-- TODO: Have params for passing table configurations
-- TODO: Handle each type of table key
-- TODO: Transform table keys into a search trie

-- matchKinds <- fetch GetTopLevelMatchKind
-- let kind = fromJust $ matchKinds ^. at (matchType ^. #name)
-- let ty = case kind ^. #name of
--             "exact"   -> C.TypedefName "_Bool"
--             "ternary" ->

generateTableKeyType :: CompC r => [AST.KeyElement] -> Sem r C.TypeSpec
generateTableKeyType elems = do
  types <- mapM (\e -> fst <$> (generateP4Type $ gdrillField @"type_" $ e ^. #expression)) elems
  pure $ C.StructDecln Nothing (fromList $ [ C.FieldDecln (C.TypeSpec ty) ("_" <> show n)
                                           | (ty, n) <- zip types [0 :: Int ..]])

generateTableCall :: (CompC r, Member (Writer [C.BlockItem]) r) => AST.TypeTable -> Text -> AST.TypeStruct -> Sem r C.Expr
generateTableCall tty name rty = do
  -- XXX: just bad lol
  let rty' = rty & #fields %~ filterMapVec ((/= "action_run") . (^. #name))
  (rty'', _) <- generateP4Type $ AST.TypeStruct'P4Type rty'
  keyTypes <- generateTableKeyType $ tty ^. #table . #keys
  undefined
