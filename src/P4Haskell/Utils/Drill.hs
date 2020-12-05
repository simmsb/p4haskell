-- |
module P4Haskell.Utils.Drill
  ( gdrillField,
  )
where

import Control.Lens
import Data.Generics.Product.Fields
import Generics.SOP as GS

class HasField'F field a s | s field -> a where
  field'f :: Lens s s a a

instance HasField' field s a => HasField'F field a s where
  field'f = field' @field @s @a

-- | Search through a SOP to find the first field
gdrillField :: forall field s a. (GS.Generic s, All2 (HasField'F field a) (Code s)) => s -> a
gdrillField s = case gdrillField' @field @a (GS.from s) of
  [x] -> x
  _ -> error "gdrillField 0 or more than 1 result"

gdrillField' :: forall field a xss. GS.All2 (HasField'F field a) xss => SOP I xss -> [a]
gdrillField' (SOP sop) = hcollapse $ hcliftA (allp (Proxy @field) (Proxy @a)) (goGet @field @a) sop

goGet :: forall field a xs. GS.All (HasField'F field a) xs => NP I xs -> K [a] xs
goGet = K . hcollapse . hcliftA (p (Proxy @field) (Proxy @a)) (K . (^. field'f @field @a) . unI)

p :: Proxy field -> Proxy a -> Proxy (HasField'F field a)
p _ _ = Proxy

allp :: Proxy field -> Proxy a -> Proxy (GS.All (HasField'F field a))
allp _ _ = Proxy
