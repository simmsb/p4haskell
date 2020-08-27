-- |
module P4Haskell.Compile.Rules
  ( rules,
  )
where

import Data.Generics.Sum
import P4Haskell.Compile.Codegen
import P4Haskell.Compile.Query
import qualified P4Haskell.Types.AST as AST
import P4Haskell.Utils.Drill
import Relude (error)
import Relude.Unsafe (fromJust)
import qualified Rock

rules :: AST.P4Program -> Rock.Rules Query
rules ast GetMain =
  (ast ^. #objects)
    & mapMaybe
      ( \o -> do
          decl <- o ^? _Typed @AST.DeclarationInstance
          guard (decl ^. #name == "main")
          pure decl
      )
    & listToMaybe
    & fromJust
    & pure
rules ast GetTopLevelTypes =
  (ast ^. #objects)
    & mapMaybe (^? _Typed @AST.TopLevelTypeDecl)
    & map (\v -> (gdrillField @"name" v, v))
    & fromList
    & pure
rules ast GetTopLevelControl =
  (ast ^. #objects)
    & mapMaybe (^? _Typed @AST.P4Control)
    & map (\v -> (v ^. #name, v))
    & fromList
    & pure
rules _ast (FetchType name) = do
    tl <- Rock.fetch GetTopLevelTypes
    case tl ^. at name of
      Just ty -> pure . injectSub $ ty
      Nothing -> error $ "The type: " <> name <> " couldn't be found"
rules _ast (GenerateP4Type t) = generateP4TypePure t
