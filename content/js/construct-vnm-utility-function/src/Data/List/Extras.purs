module Data.List.Extras where

import Data.List (List(..), (:))
import Data.List as List

tails :: forall a. List a -> List (List a)
tails Nil = List.singleton Nil
tails as'@(Cons a as) = as' : tails as
