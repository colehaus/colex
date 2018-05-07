module Economics.Utility.Ratio where

import Prelude

import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)

newtype Pair a = MkPair { base :: a, quote :: a }
derive instance newtypePair :: Newtype (Pair a) _
derive instance genericPair :: Generic a => Generic (Pair a)
derive instance eqPair :: Eq a => Eq (Pair a)
-- | WARNING: Not a semantic instance. Just so we can put it inside a map.
derive instance ordPair :: Ord a => Ord (Pair a)
instance showPair :: Generic a => Show (Pair a) where
  show = gShow

newtype Ratio a n = MkRatio { pair :: Pair a, relativeValue :: n }
derive instance newtypeRatio :: Newtype (Ratio a n) _
derive instance genericRatio :: (Generic a, Generic n) => Generic (Ratio a n)
derive instance eqRatio :: (Eq a, Eq n) => Eq (Ratio a n)
-- | WARNING: Not a semantic instance. Just so we can put it inside a map.
derive instance ordRatio :: (Ord a, Ord n) => Ord (Ratio a n)
instance showRatio :: (Generic a, Generic n) => Show (Ratio a n) where
  show = gShow

base :: forall a n. Ratio a n -> a
base (MkRatio { pair: MkPair { base } }) = base

quote :: forall a n. Ratio a n -> a
quote (MkRatio { pair: MkPair { quote } }) = quote
