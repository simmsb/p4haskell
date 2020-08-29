-- | Things that are declared while compiling
module P4Haskell.Compile.Declared
  ( Declared (..),
    declareType,
    getType,
    defineFunc,
    exportDeclared,
  )
where

import qualified Language.C99.Simple as C
import qualified Data.Map.Ordered as O

data Declared = Declared
  { declaredTypes :: O.OMap Text C.TypeSpec,
    declaredFuncs :: HashMap Text C.FunDef
  }
  deriving (Generic)

instance Semigroup Declared where
  Declared lt lf <> Declared rt rf = Declared (lt O.|<> rt) (lf <> rf)

instance Monoid Declared where
  mempty = Declared O.empty mempty

declareType :: Text -> C.TypeSpec -> Declared
declareType n v = Declared (O.singleton (n, v)) mempty

getType :: Text -> Declared -> Maybe C.TypeSpec
getType name = (O.lookup name) . (^. #declaredTypes)

defineFunc :: Text -> C.Type -> [C.Param] -> [C.BlockItem] -> Declared
defineFunc n ty params body = mempty & #declaredFuncs . at n ?~ C.FunDef ty (toString n) params body

exportDeclared :: Declared -> C.TransUnit
exportDeclared d = C.TransUnit (map (C.TypeDecln . C.TypeSpec) $ d ^.. #declaredTypes . traverse) (d ^.. #declaredFuncs . traverse)
