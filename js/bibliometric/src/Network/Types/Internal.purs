module Network.Types.Internal where

import Control.Monad.Error.Class
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Tuple

import Math.Probability

data Space a b = Space a [b]
data CondPMF a b c d = CondPMF a c (M.Map d (Dist b))
data CondPMFN b c d = CondPMFN c (M.Map d (Dist b))
newtype Network a b c d = Network (M.Map a (CondPMFN b c d))
newtype PmfError = PmfError String

cpn (CondPMF _ c m) = CondPMFN c m
cp a (CondPMFN c m) = CondPMF a c m

netList (Network m) = uncurry cp <$> M.toList m

combos :: forall a. (Ord a) => [[a]] -> [[a]]
combos = T.sequence <<< (<$>) setNub <<< transpose

-- Assumes all lists are same length
transpose :: forall a. [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = A.map AU.head rows : transpose (A.map AU.tail rows)

duplicate :: forall a b. (Ord a) => [a] -> Boolean
duplicate as = A.length as /= A.length (setNub as)

throwPmf :: forall a m. (MonadError PmfError m) => String -> m a
throwPmf = throwError <<< PmfError

setNub :: forall a. (Ord a) => [a] -> [a]
setNub = S.toList <<< S.fromList

-- Orphan instances

instance ordSet :: (Ord a) => Ord (S.Set a) where
  compare a b = compare (S.toList a) (S.toList b)

instance ordMap :: (Ord k, Ord v) => Ord (M.Map k v) where
  compare a b = compare (M.toList a) (M.toList b)

-- Derivable boilerplate

instance eqSpace :: (Eq a, Eq b) => Eq (Space a b) where
  (==) (Space va psa) (Space vb psb) = va == vb && psa == psb
  (/=) a b = not $ a == b
instance ordSpace :: (Ord a, Ord b) => Ord (Space a b) where
  compare (Space va psa) (Space vb psb) = compare va vb <> compare psa psb
instance showSpace :: (Show a, Show b) => Show (Space a b) where
  show (Space v ps) = "Space (" <> show v <> ") (" <> show ps <> ")"

instance eqCond :: (Eq a, Eq b, Eq c, Eq d) => Eq (CondPMF a b c d) where
  (==) (CondPMF sa vsa ma) (CondPMF sb vsb mb) =
    sa == sb && vsa == vsb && ma == mb
  (/=) a b = not $ a == b
instance ordCond :: (Ord a, Ord b, Ord c, Ord d) => Ord (CondPMF a b c d) where
  compare (CondPMF sa vsa ma) (CondPMF sb vsb mb) =
    compare sa sb <> compare vsa vsb <> compare ma mb
instance showCond :: (Show a, Show b, Show c, Show d) =>
         Show (CondPMF a b c d) where
  show (CondPMF s vs m) =
    "CondPMF (" <> show s <> ") (" <> show vs <> ") (" <> show m <> ")"

instance eqCondN :: (Eq b, Eq c, Eq d) => Eq (CondPMFN b c d) where
  (==) (CondPMFN vsa ma) (CondPMFN vsb mb) = vsa == vsb && ma == mb
  (/=) a b = not $ a == b
instance ordCondN :: (Ord b, Ord c, Ord d) => Ord (CondPMFN b c d) where
  compare (CondPMFN vsa ma) (CondPMFN vsb mb) = compare vsa vsb <> compare ma mb

instance eqNet :: (Eq a, Eq b, Eq c, Eq d) => Eq (Network a b c d) where
  (==) (Network a) (Network b) = a == b
  (/=) a b = not $ a == b
instance ordNet :: (Ord a, Ord b, Ord c, Ord d) => Ord (Network a b c d) where
  compare (Network a) (Network b) = compare a b
instance showNet :: (Show a, Show b, Show c, Show d) =>
         Show (Network a b c d) where
  show n = "Network " <> show (netList n)

instance showPmfError :: Show PmfError where
  show (PmfError p) = p
