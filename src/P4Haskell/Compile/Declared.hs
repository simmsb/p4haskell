-- | Things that are declared while compiling
module P4Haskell.Compile.Declared (
  Declared (..),
  declareType,
  getType,
  defineFunc,
  defineStatic,
  exportDeclared,
) where

import Control.Lens
import Data.Generics.Labels ()
import qualified Data.Map.Ordered as O
import qualified Language.C99.Simple as C
import Relude

data Declared = Declared
  { declaredTypes :: O.OMap Text C.TypeSpec
  , declaredFuncs :: O.OMap Text C.FunDef
  , declaredStatics :: O.OMap Text C.Decln
  }
  deriving stock (Generic)

instance Semigroup Declared where
  Declared lt lf ls <> Declared rt rf rs = Declared (lt O.|<> rt) (lf O.|<> rf) (ls O.|<> rs)

instance Monoid Declared where
  mempty = Declared O.empty O.empty O.empty

declareType :: Text -> C.TypeSpec -> Declared
declareType n v = Declared (O.singleton (n, v)) O.empty O.empty

getType :: Text -> Declared -> Maybe C.TypeSpec
getType name = O.lookup name . (^. #declaredTypes)

fixupBody :: [C.BlockItem] -> [C.BlockItem]
fixupBody [] = [C.Stmt . C.Expr . C.LitInt $ 0]
fixupBody xs = xs

defineFunc :: Text -> C.Type -> [C.Param] -> [C.BlockItem] -> Declared
defineFunc n ty params body = mempty & #declaredFuncs .~ O.singleton (n, C.FunDef ty (toString n) params (fixupBody body))

defineStatic :: Text -> Maybe C.StorageSpec -> C.Type -> C.Init -> Declared
defineStatic n sp ty ini = mempty & #declaredStatics .~ O.singleton (n, C.VarDecln sp ty (toString n) (Just ini))

exportDeclared :: Declared -> C.TransUnit
exportDeclared d =
  let types = map (C.TypeDecln . C.TypeSpec) $ d ^.. #declaredTypes . traverse
      statics = d ^.. #declaredStatics . traverse
      funcs = d ^.. #declaredFuncs . traverse
   in C.TransUnit (types <> statics) funcs
