module Economics.Utility.VNM where

import Prelude hiding (bottom,top)

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Interval.Bound (Bound(..))
import Data.Interval.Internal (Interval(..))
import Data.Interval.Openness (Openness(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(Tuple), fst, snd)
import Economics.Utility.Ratio (Ratio(MkRatio))
import Economics.Utility.VNM.Function (UtilityFn, unmake)
import Economics.Utility.VNM.Function as Function
import Math (sqrt)
import Math.Interval (boundAbove, boundBelow, normalizedWidth)
import Math.Interval as Interval
import Partial.Unsafe (unsafeCrashWith)

foreign import top :: Number
foreign import smallest :: Number
bottom :: Number
bottom = -top

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
geometricMidpoint (Finite { bound: 0.0 }) (Finite r) = smallest
geometricMidpoint (Finite l) (Finite r)
  | l.bound < 0.0 && r.bound < 0.0 = geometricMean (-l.bound) (-r.bound)
  | otherwise = geometricMean l.bound r.bound

widest ::
     forall a n.
     EuclideanRing n
  =>  Ord a
  => Ord n
  => Ring n
  => NonEmpty Set (Tuple a (Interval n))
  -> Tuple a (Interval n)
widest =
  unsafeFromJustBecause "`NonEmpty`" <<<
  maximumBy (compare `on` (normalizedWidth <<< snd)) <<< fromNonEmpty Set.insert

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

unzip :: forall t b a. Functor t => t (Tuple a b) -> Tuple (t a) (t b)
unzip xs = Tuple (fst <$> xs) (snd <$> xs)

pickNextLottery ::
     forall a.
     Ord a
  => UtilityFn a Number
  -> Ratio a Number
pickNextLottery fn =
 case widest <<< unmake $ fn of
    Tuple _ Empty -> unsafeCrashWith "Our intervals should never be empty"
    Tuple pair (NonEmpty {lower, upper}) ->
      MkRatio { pair, relativeValue : geometricMidpoint lower upper }

refine ::
     forall a n.
     Ord a
  => Ord n
  => Ratio a n
  -> Ordering
  -> UtilityFn a n
  -> UtilityFn a n
refine (MkRatio ratio) EQ =
  Function.update ratio.pair (const $ Interval.singleton ratio.relativeValue)
refine (MkRatio ratio) GT =
  Function.update
    ratio.pair
    (unsafeFromJustBecause "Midpoint guaranteed within interval" <<<
     boundBelow ratio.relativeValue Open)
refine (MkRatio ratio) LT =
  Function.update
    ratio.pair
    (unsafeFromJustBecause "Midpoint guaranteed within interval" <<<
     boundAbove ratio.relativeValue Open)

nonEmptySet :: forall a. Ord a => Set a -> Maybe (NonEmpty Set a)
nonEmptySet s = (\m -> m :| m `Set.delete` s) <$> Set.findMin s
