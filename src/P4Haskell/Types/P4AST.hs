module P4Haskell.Types.P4AST
    ( fixupTree ) where

import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Lazy as H
import           Data.Maybe

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding        = TaggedObject "Node_Type" "Node_Type"
                             , omitNothingFields  = True
                             , fieldLabelModifier = filter (/= '_') }

-- | the secret sauce
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go
  where
    go = fmap ($ go) x

-- | recursively fetches every object in a json value
objects :: (Plated a, AsValue a) => a -> [Object]
objects v = universe v ^.. traverse . _Object

pattern NodeID :: forall s. (At s, IsString (Index s), AsNumber (IxValue s)) => Integer -> s
pattern NodeID n <- (preview (at "Node_ID" . _Just . _Integer) -> Just n)

-- | is an object in the format "{\"Node_ID\": 3}"?
isReference :: Object -> Bool
isReference (H.toList -> [("Node_ID", _)]) = True
isReference _ = False

-- | get the ID of an object
fetchID :: Object -> Maybe Integer
fetchID (NodeID n) = Just n
fetchID _ = Nothing

-- | fetches all non-reference objects recursively from a json value
nonReferenceObjects :: (Plated a, AsValue a) => a -> [Object]
nonReferenceObjects = objects >>> filter (not . isReference)

replaceNode :: Value -> H.HashMap Integer Object -> Value
replaceNode (Object (NodeID n)) (H.lookup n -> Just v) = Object v
replaceNode v _ = v

replaceWithThunks :: Value -> H.HashMap Integer Object -> Value
replaceWithThunks (Object o@(NodeID n))
  | isReference o = H.lookup n >>> fromJust >>> Object
replaceWithThunks v = const v

-- | replace all reference objects in a json value with their full versions
fixupTree :: Value -> Value
fixupTree v = let completeObjects = nonReferenceObjects v
                    & mapMaybe (\o -> do
                                  n <- fetchID o
                                  pure (n, (\(Object o') -> o') . transformM replaceWithThunks (Object o)))
                    & H.fromList
                    & loeb
              in transform (`replaceNode` completeObjects) v
