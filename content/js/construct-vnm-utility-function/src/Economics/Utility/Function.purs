module Economics.Utility.VNM.Function where

import Prelude

import Data.Either (Either(Right, Left))
import Data.Filterable (filter)
import Data.Foldable (length)
import Data.Foldable as Foldable
import Data.Generic (class Generic)
import Data.List (List(..), (:))
import Data.List.Extras (tails)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Economics.Utility.Ratio (Pair(..), Ratio(..))
import Math.Interval ((<=!), (>=!))
import Math.Interval as Interval
import Math.Interval.Bound (Bound(..))
import Math.Interval.Internal (Interval(..))
import Math.Interval.Openness (Openness(..))
import Partial.Unsafe (unsafeCrashWith)

newtype UtilityFn a n = MkUtilityFn (Map (Pair a) (Interval n))
derive newtype instance showUtilityFn :: (Generic a, Generic n) => Show (UtilityFn a n)

goods :: forall a n. Ord a => UtilityFn a n -> Set a
goods (MkUtilityFn fn) =
  Set.fromFoldable <<<
  (_ >>= \(MkPair {quote, base}) -> quote : base : Nil) <<< Map.keys $
  fn

pairs :: forall a n. Ord a => UtilityFn a n -> Set (Pair a)
pairs (MkUtilityFn m) = Set.fromFoldable $ Map.keys m

pairCombos :: forall a. List a -> List (Tuple a a)
pairCombos l = do
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
    pairCombos <<< Set.toUnfoldable <<< fromNonEmpty Set.insert $
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
  => UtilityFn a n -> Indexed.NonEmpty Map (Pair a) (Interval n)
unmake (MkUtilityFn m) =
  unsafeFromJustBecause "We should never have an empty set of pairs" <<<
  nonEmptyMap $
  m
  where
    asList :: forall b. List b -> List b
    asList = id

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

nonEmptySet :: forall a. Ord a => Set a -> Maybe (NonEmpty Set a)
nonEmptySet s = (\m -> m :| m `Set.delete` s) <$> Set.findMin s

nonEmptyMap ::
     forall k v.
     Ord k
  => Map k v
  -> Maybe (Indexed.NonEmpty Map k v)
nonEmptyMap m =
  (\l -> (Tuple l.key l.value) Indexed.:| (l.key `Map.delete` m)) <$> Map.findMin m

best :: forall a n. Ord a => Ord n => Semiring n => UtilityFn a n -> Maybe a
best =
  extractOnly <<<
  filter (_ == Set.singleton true) <<<
  Map.mapWithKey (\good ratios -> Set.map ((_ == Just good) <<< better) ratios) <<<
  byGood
  where
    extractOnly m
      | length m > 1 =
        unsafeCrashWith "There should only ever be one 'best' good"
      | otherwise = _ . key <$> Map.findMin m

byGood ::
     forall a n.
     Ord a
  => Ord n
  => UtilityFn a n
  -> Map a (Set (Ratio a (Interval n)))
byGood (MkUtilityFn m) = Map.fromFoldableWith (<>) do
  Tuple (pair@MkPair {quote, base}) relativeValue <- Map.toUnfoldable $ m
  let ratio = MkRatio { pair, relativeValue }
  (Tuple quote (Set.singleton ratio) : Tuple base (Set.singleton ratio) : Nil)

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

-- | Prune utility function so all remaining ratios involve chosen item
keepBase :: forall a n. Ord a => a -> UtilityFn a n -> UtilityFn a n
keepBase a (MkUtilityFn m) = MkUtilityFn $ Map.filterKeys (contains a) m
  where
    contains a (MkPair { quote, base }) = quote == a || base == a

pruned :: forall a n. Ord a => Ord n => Semiring n => UtilityFn a n -> Boolean
pruned fn =
  case best fn of
    Just a ->
      Foldable.all (\(MkPair {base, quote}) -> quote == a || base == a) <<<
      pairs $
      fn
    Nothing -> false
