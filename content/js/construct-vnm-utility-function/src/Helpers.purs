module Helpers where

import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set (Set)
import Data.Set as Set
import Partial.Unsafe (unsafeCrashWith)

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

nonEmptySet :: forall a. Ord a => Set a -> Maybe (NonEmpty Set a)
nonEmptySet s = (\m -> m :| m `Set.delete` s) <$> Set.findMin s
