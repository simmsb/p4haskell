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
rules ast (FetchType t) =
  (ast ^. #objects)
    & mapMaybe
      ( \o -> do
          decl <- o ^? _Typed @AST.TopLevelTypeDecl
          guard (gdrillField @"name" decl == t)
          pure decl
      )
    & listToMaybe
    & fromJust
    & pure
rules _ast (GenerateP4Type t) = generateP4TypePure t
