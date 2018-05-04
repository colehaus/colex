module Economics.Utility.VNM.Function where

import Prelude

import Data.Either (Either(..), either, note)
import Data.Foldable (length)
import Data.Generic (class Generic)
import Data.Interval.Bound (Bound(..))
import Data.Interval.Internal (Interval(..))
import Data.Interval.Openness (Openness(..))
import Data.List (List(..))
import Data.List.Extras (tails)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.NonEmpty.Extras as NonEmpty
import Data.Semigroup.Foldable (fold1)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Economics.Utility.Ratio (Pair(..), Ratio(..))
import Math.Interval ((<=!), (>=!))
import Math.Interval as Interval
import Partial.Unsafe (unsafeCrashWith)

newtype UtilityFn a n = MkUtilityFn (Map (Pair a) (Interval n))
derive newtype instance showUtilityFn :: (Generic a, Generic n) => Show (UtilityFn a n)

pairs :: forall a. List a -> List (Tuple a a)
pairs l = do
  ls <- tails l
  case ls of
    Nil -> Nil
    Cons x ys -> do
      y <- ys
      pure $ Tuple x y

-- | Must pass in a set with at least two elements
goodsToInitialFn ::
     forall a n.
     Ord a
  => Ord (Pair a)
  => Ord n
  => Semiring n
  => NonEmpty Set a
  -> Either Unit (UtilityFn a n)
goodsToInitialFn set
  | length set == 1 = Left unit
  | otherwise =
    Right <<<
    MkUtilityFn <<<
    Map.fromFoldable <<<
    map ((flip Tuple positive) <<< mkPair) <<<
    pairs <<< Set.toUnfoldable <<< fromNonEmpty Set.insert $
    set
      where
        positive
          = NonEmpty
          { lower: Finite { bound: zero, openness: Open }
          , upper: PosInf
          }
        mkPair (Tuple base quote) = MkPair {base, quote}

unmake ::
     forall n a.
     Ord a
  => Ord n
  => UtilityFn a n -> NonEmpty Set (Tuple (Pair a) (Interval n))
unmake (MkUtilityFn m) =
  unsafeFromJustBecause "We should never have an empty set of pairs" <<<
  nonEmptySet <<< Set.fromFoldable <<< asList <<< Map.toUnfoldable $
  m
  where
    asList :: forall b. List b -> List b
    asList = id

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

nonEmptySet :: forall a. Ord a => Set a -> Maybe (NonEmpty Set a)
nonEmptySet s = (\m -> m :| m `Set.delete` s) <$> Set.findMin s

best :: forall a n. Ord a => Ord n => Semiring n => UtilityFn a n -> Maybe a
best =
  either (const Nothing) extractOnly <<<
  fold1 <<<
  NonEmpty.map Set.map (note unit <<< map Set.singleton <<< better <<< mkRatio) <<<
  unmake
  where
    extractOnly s
      | Set.size s == 1 = Set.findMin s
      | otherwise = Nothing
    mkRatio (Tuple pair relativeValue) = MkRatio { pair, relativeValue }
    asList :: forall b. List b -> List b
    asList = id

better :: forall a n. Ord n => Semiring n => Ratio a (Interval n) -> Maybe a
better (MkRatio { pair: (MkPair { base, quote }), relativeValue })
  | relativeValue <=! Interval.singleton one = Just quote
  | relativeValue >=! Interval.singleton one = Just base
  | otherwise = Nothing

worse :: forall a n. Ord n => Semiring n => Ratio a (Interval n) -> Maybe a
worse (MkRatio { pair: (MkPair { base, quote }), relativeValue })
  | relativeValue <=! Interval.singleton one = Just base
  | relativeValue >=! Interval.singleton one = Just quote
  | otherwise = Nothing

update :: forall a n. Ord a => Pair a -> (Interval n -> Interval n) -> UtilityFn a n -> UtilityFn a n
update key f (MkUtilityFn m) = MkUtilityFn $ Map.update (Just <<< f) key m
