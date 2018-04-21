module Economics.Utility.VNM where

import Prelude hiding (bottom,top)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Generic (class Generic, gShow)
import Data.Interval (boundAbove, boundBelow, width)
import Data.Interval as Interval
import Data.Interval.Bound (Bound(..))
import Data.Interval.Internal (Interval(..))
import Data.Interval.Openness (Openness(..))
import Data.List (List(..))
import Data.List.Extras (tails)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.NonEmpty.Indexed (deindex)
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Math (sqrt)
import Partial.Unsafe (unsafeCrashWith)

pairs :: forall a. List a -> List (Tuple a a)
pairs l = do
  ls <- tails l
  case ls of
    Nil -> Nil
    Cons x ys -> do
      y <- ys
      pure $ Tuple x y

newtype Pair a = MkPair { base :: a, quote :: a }
derive instance newtypePair :: Newtype (Pair a) _
derive instance genericPair :: Generic a => Generic (Pair a)
derive instance eqPair :: Eq a => Eq (Pair a)
-- WARNING: Not a semantic instance. Just so we can put it inside a map.
derive instance ordPair :: Ord a => Ord (Pair a)
instance showPair :: Generic a => Show (Pair a) where
  show = gShow

newtype Ratio a n = MkRatio { pair :: Pair a, relativeValue :: n }
derive instance newtypeRatio :: Newtype (Ratio a n) _
derive instance genericRatio :: (Generic a, Generic n) => Generic (Ratio a n)
derive instance eqRatio :: (Eq a, Eq n) => Eq (Ratio a n)
instance showRatio :: (Generic a, Generic n) => Show (Ratio a n) where
  show = gShow

foreign import top :: Number
bottom :: Number
bottom = -top

nonEmptyMap ::
     forall k v.
     Ord k
  => Map k v
  -> Maybe (Indexed.NonEmpty Map k v)
nonEmptyMap m =
  (\l -> (Tuple l.key l.value) Indexed.:| (l.key `Map.delete` m)) <$> Map.findMin m

initial ::
     forall a n.
     Ord a
  => Ord (Pair a)
  => Ord n
  => Semiring n
  => NonEmpty Set a
  -> Indexed.NonEmpty Map (Pair a) (Interval n)
initial =
  unsafeFromJustBecause "initial" <<< nonEmptyMap <<< Map.fromFoldable <<<
  map ((flip Tuple positive) <<< mkPair) <<< pairs <<< Set.toUnfoldable <<< fromNonEmpty Set.insert
  where
    positive
      = NonEmpty
      { lower: Finite { bound: un Additive mempty, openness: Open }
      , upper: PosInf
      }
    mkPair (Tuple base quote) = MkPair {base, quote}

geometricMean :: Number -> Number -> Number
geometricMean l r = sqrt l * sqrt r

geometricMidpoint :: Bound Number -> Bound Number -> Number
geometricMidpoint NegInf PosInf = 0.0
geometricMidpoint NegInf (Finite { bound })
  | bound > -1.0 = -1.0
  | otherwise = geometricMean (-bound) (-bottom)
geometricMidpoint PosInf r = geometricMidpoint r PosInf
geometricMidpoint l NegInf = geometricMidpoint NegInf l
geometricMidpoint (Finite { bound }) PosInf
  | bound < 1.0 = 1.0
  | otherwise = geometricMean bound top
geometricMidpoint (Finite l) (Finite r)
  | l.bound < 0.0 && r.bound < 0.0 = geometricMean (-l.bound) (-r.bound)
  | otherwise = geometricMean l.bound r.bound

widest ::
     forall a n.
     Ord n
  => Ring n
  => NonEmpty List (Tuple a (Interval n))
  -> Tuple a (Interval n)
widest =
  unsafeFromJustBecause "widest" <<<
  maximumBy (compare `on` (width <<< snd)) <<< fromNonEmpty Cons

refine :: forall n. Ord n => n -> Ordering -> Interval n -> Interval n
refine n EQ = const $ Interval.singleton n
refine n LT = unsafeFromJustBecause "refine LT" <<< boundAbove n Open
refine n GT = unsafeFromJustBecause "refine GT" <<< boundBelow n Open

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

print ::
     forall a e n.
     Show a
  => Indexed.NonEmpty Map a (Interval Number)
  -> Eff (console :: CONSOLE | e) (Tuple (Tuple a (Interval Number)) Number)
print ps = do
  let w = widest <<< deindex id Map.toUnfoldable $ ps
  case snd w of
    Empty -> unsafeCrashWith "empty"
    (NonEmpty { lower, upper }) -> do
      log <<< show <<< fst $ w
      let mid = geometricMidpoint lower upper
      log <<< show $ mid
      pure $ Tuple w mid

update ::
     forall a n.
     Ord a
  => Ord n
  => Tuple (Tuple a (Interval n)) n
  -> Ordering
  -> Indexed.NonEmpty Map a (Interval n)
  -> Indexed.NonEmpty Map a (Interval n)
update (Tuple (Tuple k i) n) o =
  unsafeFromJustBecause "update" <<<
  nonEmptyMap <<<
  Map.insert k (refine n o i) <<< Indexed.fromNonEmpty Map.insert

nonEmptySet :: forall a. Ord a => Set a -> Maybe (NonEmpty Set a)
nonEmptySet s = (\m -> m :| m `Set.delete` s) <$> Set.findMin s

setup ::
     forall n.
     Ord n
  => Semiring n
  => Indexed.NonEmpty Map (Pair String) (Interval n)
setup =
  initial <<< unsafeFromJustBecause "main" <<< nonEmptySet <<< Set.fromFoldable $
  ["a", "b", "c"]
