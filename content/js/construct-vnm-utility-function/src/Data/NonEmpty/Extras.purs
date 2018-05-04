module Data.NonEmpty.Extras where

import Data.NonEmpty (NonEmpty, (:|))

map :: forall a b f. ((a -> b) -> f a -> f b) -> (a -> b) -> NonEmpty f a -> NonEmpty f b
map mapper f (a :| as) = (f a :| mapper f as)
