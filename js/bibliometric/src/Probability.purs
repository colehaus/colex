module Probability where

import Control.Arrow
import Control.Bind
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid.All
import Data.Monoid.Additive
import Data.Tuple
import Math

type Event a = a -> Boolean

oneOf :: forall a. (Eq a) => [a] -> Event a
oneOf = flip F.elem

just :: forall a. (Eq a) => a -> Event a
just = (==)

newtype Dist a = Dist [Tuple a Number]
runDist (Dist a) = a
distProbs :: forall a. Dist a -> ProbList
distProbs (Dist a) = ProbList $ Prob <<< snd <$> a


instance functorDist :: Functor Dist where
  (<$>) f = Dist <<< (<$>) (f *** id) <<< runDist

instance applyDist :: Apply Dist where
  (<*>) d a = Dist $ do
    (Tuple f p) <- runDist $ d
    (Tuple b q) <- runDist $ a
    pure $ Tuple (f b) (p * q)

instance applicativeDist :: Applicative Dist where
  pure x = Dist <<< A.singleton $ Tuple x 1

instance bindDist :: Bind Dist where
  (>>=) d f = Dist $ do
    (Tuple a p) <- runDist $ d
    (Tuple b q) <- runDist $ f a
    pure $ Tuple b (p * q)

instance monadDist :: Monad Dist

epsilon :: Number
epsilon = 0.0005

infix 4 ~~
(~~) :: Number -> Number -> Boolean
(~~) a b = abs (a - b) < epsilon
infix 4 /~
(/~) :: Number -> Number -> Boolean
(/~) a b = not $ a ~~ b
infix 4 <~
(<~) :: Number -> Number -> Boolean
(<~) a b = a - b < epsilon
infix 4 >~
(>~) :: Number -> Number -> Boolean
(>~) a b = b - a < epsilon

approx :: forall a. (Ord a) => Dist a -> Dist a -> Boolean
approx (Dist xs) (Dist ys) =
  runAll <<< F.mconcat $
  A.zipWith (\(Tuple x p) (Tuple y q) -> All $ x == y && p ~~ q) xs ys

lift :: forall a. ([Tuple a Number] -> [Tuple a Number]) -> Dist a -> Dist a
lift f  = Dist <<< f <<< runDist

size :: forall a. Dist a -> Number
size = A.length <<< runDist

isValid :: forall a. [Tuple a Number] -> Boolean
isValid = (~~) 1 <<< sumP

dist :: forall a. [Tuple a Prob] -> Maybe (Dist a)
dist d =
  if isValid d'
  then Just $ Dist d'
  else Nothing where
  d' = (id *** runProb) <$> d

zipDist :: forall a. [a] -> ProbList -> Dist a
zipDist as (ProbList ps) = Dist $ A.zipWith (\a (Prob p) -> Tuple a p) as ps

sumP :: forall a. [Tuple a Number] -> Number
sumP = F.sum <<< (<$>) snd

sortP :: forall a. [Tuple a Number] -> [Tuple a Number]
sortP = A.sortBy (compare `on` snd)

sortElem :: forall a. (Ord a) => [Tuple a Number] -> [Tuple a Number]
sortElem = A.sortBy (compare `on` fst)

norm :: forall a. (Ord a) => Dist a -> Dist a
norm = lift norm'

norm' :: forall a. (Ord a) => [Tuple a Number] -> [Tuple a Number]
norm' = M.toList <<< M.fromListWith (+)

-- Purescript compiler does not like this:
-- instance semiringDist :: (Semiring a, Ord a) => Semiring (Dist a) where
--   (+) x y = norm $ (+) <$> x <*> y
--   (*) x y = norm $ (*) <$> x <*> y
--   one = pure one
--   zero = pure zero
-- instance moduloSemiringDist :: (ModuloSemiring a, Ord a) => ModuloSemiring a where
--   (/) x y = norm $ (/) <$> x <*> y
--   mod x y = norm $ mod <$> x <*> y
-- instance ringDist :: (Ring a, Ord a) => Ring (Dist a) where
--   (-) x y = norm $ (-) <$> x <*> y
-- instance divisionDist :: (DivisionRing a, Ord a) => DivisionRing (Dist a)
-- instance numDist :: (Num a, Ord a) => Num (Dist a)

type Spread a = [a] -> Maybe (Dist a)

choose :: forall a. Prob -> a -> a -> Dist a
choose p x y = let p' = runProb p in Dist [Tuple x p', Tuple y (1-p')]

relative :: forall a. [Number] -> Spread a
relative ns = fromFreqs <<< flip zip ns

fromFreqs :: forall a. [Tuple a Number] -> Maybe (Dist a)
fromFreqs xs = let q = sumP xs in dist $ (id *** (Prob <<< (/ q))) <$> xs

uniform :: forall a. Spread a
uniform = fromFreqs <<< (<$>) (\a -> Tuple a 1)

reshape :: forall a. Spread a -> Dist a -> Maybe (Dist a)
reshape s = s <<< extract

-- shape :: (Number -> Number) -> Spread a
-- shape _ [] = error "Probability.shape: empty list"
-- shape f xs =
--    let incr = 1 / fromIntegral (length xs - 1)
--        ps = List.map f (iterate (+incr) 0)
--    in  fromFreqs (zip xs ps)

-- linear :: Fractional prob => Spread prob a
-- linear = shape Shape.linear

-- uniform :: Fractional prob => Spread prob a
-- uniform = shape Shape.uniform

-- negExp :: Floating prob => Spread prob a
-- negExp = shape Shape.negExp

-- normal :: Floating prob => Spread prob a
-- normal = shape Shape.normal

extract :: forall a. Dist a -> [a]
extract = (<$>) fst <<< runDist

map :: forall a b. (Ord b) => (a -> b) -> Dist a -> Dist b
map f = norm <<< (<$>) f

join' :: forall a. (Ord a) => Dist (Dist a) -> Dist a
join' = norm <<< join

cond :: forall a. Dist Boolean-> Dist a -> Dist a -> Dist a
cond b d d' = b >>= \c -> if c then d else d'

infixl 1 >>=?
infixr 1 ?=<<
(?=<<) :: forall a. (a -> Boolean) -> Dist a -> Maybe (Dist a)
(?=<<) = filter
(>>=?) :: forall a. Dist a -> (a -> Boolean) -> Maybe (Dist a)
(>>=?) = flip filter

filter :: forall a. (a -> Boolean) -> Dist a -> Maybe (Dist a)
filter p = fromFreqs <<< A.filter (p <<< fst) <<< runDist

mapMaybe :: forall a b. (a -> Maybe b) -> Dist a -> Maybe (Dist b)
mapMaybe f =
  fromFreqs <<< A.mapMaybe (\(Tuple a p) -> flip Tuple p <$> f a) <<< runDist

newtype Prob = Prob Number
runProb (Prob p) = p
prob :: Number -> Maybe Prob
prob n | 0 <~ n && n <~ 1 = Just $ Prob n
prob n | otherwise = Nothing

infixr 1 ??
(??) :: forall a. Event a -> Dist a -> Prob
(??) p = Prob <<< sumP <<< A.filter (p <<< fst) <<< runDist

data Iso a b = Iso (a -> b) (b -> a)
to :: forall a b. Iso a b -> a -> b
to (Iso f _) = f
from :: forall a b. Iso a b -> b -> a
from (Iso _ f) = f

joinDists :: forall a b c. (a -> b -> c) -> Dist a -> (a -> Dist b) -> Dist c
joinDists f as bs'a = do
  a <- as
  b <- bs'a a
  pure $ f a b

marginalize :: forall a b. (Eq b) => (a -> b) -> Dist a -> Dist b
marginalize f d =
  fromJust <<< dist $ (<$>) (\b -> Tuple b $ ((==) b <<< f) ?? d) <<<
  A.nub $ f <$> extract d

expected :: forall a. Iso a Number -> Dist a -> a
expected i = from i <<< F.sum <<< (<$>) (\(Tuple a b) -> to i a * b) <<< runDist

variance :: forall a. Iso a Number -> Dist a -> a
variance i xs =
  from i <<< expected i' $ (\x -> pow (x - m) 2) <$> xs' where
    i' = (Iso id id)
    m = expected i' xs'
    xs' = to i <$> xs

stdDev :: forall a. Iso a Number -> Dist a -> a
stdDev i = from i <<< sqrt <<< to i <<< variance i

data ProbList = ProbList [Prob]
probList :: [Prob] -> Maybe ProbList
probList ps =
  if F.sum (runProb <$> ps) ~~ 1
  then Just $ ProbList ps
  else Nothing
runProbList (ProbList ps) = ps

foreign import undefined :: forall a. a

instance eqProb :: Eq Prob where
  (==) (Prob a) (Prob b) = a == b
  (/=) a b = not $ a == b
instance ordProb :: Ord Prob where
  compare (Prob a) (Prob b) = compare a b
instance showProb :: Show Prob where
  show (Prob a) = show a

instance showDist :: (Show a) => Show (Dist a) where
  show (Dist a) = "Dist " <> show a
instance eqDist :: (Eq a) => Eq (Dist a) where
  (==) (Dist a) (Dist b) = a == b
  (/=) a b = not $ a == b
instance ordDist :: (Ord a) => Ord (Dist a) where
  compare (Dist a) (Dist b) = a `compare` b

instance eqProbList :: Eq ProbList where
  (==) (ProbList a) (ProbList b) = a == b
  (/=) a b = not $ a == b
instance ordProbList :: Ord ProbList where
  compare (ProbList a) (ProbList b) = a `compare` b
instance showProbList :: Show ProbList where
  show (ProbList a) = "ProbList (" <> show a <> ")"

