module Network.Types.Internal where

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Set as S
import Data.Traversable as T
import Partial.Unsafe (unsafePartial)
import Prelude

import Math.Probability (Dist)

data Space a b = Space a (Array b)
data CondPMF a b c d = CondPMF a c (M.Map d (Dist b))
data CondPMFN b c d = CondPMFN c (M.Map d (Dist b))
data Network a b c d = Network (M.Map a (CondPMFN b c d)) (Array a)
newtype PmfError = PmfError String

cpn :: forall a b c d. CondPMF a d c b -> CondPMFN d c b
cpn (CondPMF _ c m) = CondPMFN c m
cp :: forall a b c d. d -> CondPMFN c b a -> CondPMF d c b a
cp a (CondPMFN c m) = CondPMF a c m

netList :: forall a b c d. Ord a => Network a b c d -> Array (CondPMF a b c d)
netList (Network m as) = unsafePartial $ (\a -> cp a <<< fromJust $ a `M.lookup` m) <$> as

combos :: forall a. Ord a => Array (Array a) -> Array (Array a)
combos = T.sequence <<< (<$>) setNub <<< transpose

-- Assumes all lists are same length
transpose :: forall a. Array (Array a) -> Array (Array a)
transpose as =
  case A.uncons as of
    Just { head: [], tail: _ } -> []
    Just { head: _, tail: _ } -> unsafePartial $ map (fromJust <<< A.head) as A.: transpose (map (fromJust <<< A.tail) as)
    Nothing -> []

duplicate :: forall a. (Ord a) => Array a -> Boolean
duplicate as = A.length as /= A.length (setNub as)

throwPmf :: forall a m. (MonadError PmfError m) => String -> m a
throwPmf = throwError <<< PmfError

setNub :: forall a. (Ord a) => Array a -> Array a
setNub = S.toUnfoldable <<< S.fromFoldable

-- Derivable boilerplate

instance eqSpace :: (Eq a, Eq b) => Eq (Space a b) where
  eq (Space va psa) (Space vb psb) = va == vb && psa == psb
instance ordSpace :: (Ord a, Ord b) => Ord (Space a b) where
  compare (Space va psa) (Space vb psb) = compare va vb <> compare psa psb
instance showSpace :: (Show a, Show b) => Show (Space a b) where
  show (Space v ps) = "Space (" <> show v <> ") (" <> show ps <> ")"

instance eqCond :: (Eq a, Eq b, Eq c, Eq d) => Eq (CondPMF a b c d) where
  eq (CondPMF sa vsa ma) (CondPMF sb vsb mb) =
    sa == sb && vsa == vsb && ma == mb
instance ordCond :: (Ord a, Ord b, Ord c, Ord d) => Ord (CondPMF a b c d) where
  compare (CondPMF sa vsa ma) (CondPMF sb vsb mb) =
    compare sa sb <> compare vsa vsb <> compare ma mb
instance showCond :: (Show a, Show b, Show c, Show d) =>
         Show (CondPMF a b c d) where
  show (CondPMF s vs m) =
    "CondPMF (" <> show s <> ") (" <> show vs <> ") (" <> show m <> ")"

instance eqCondN :: (Eq b, Eq c, Eq d) => Eq (CondPMFN b c d) where
  eq (CondPMFN vsa ma) (CondPMFN vsb mb) = vsa == vsb && ma == mb
instance ordCondN :: (Ord b, Ord c, Ord d) => Ord (CondPMFN b c d) where
  compare (CondPMFN vsa ma) (CondPMFN vsb mb) = compare vsa vsb <> compare ma mb

instance eqNet :: (Eq a, Eq b, Eq c, Eq d) => Eq (Network a b c d) where
  eq (Network a _) (Network b _) = a == b
instance ordNet :: (Ord a, Ord b, Ord c, Ord d) => Ord (Network a b c d) where
  compare (Network a _) (Network b _) = compare a b
instance showNet :: (Show a, Show b, Show c, Show d, Ord a) =>
         Show (Network a b c d) where
  show n = "Network " <> show (netList n)

instance showPmfError :: Show PmfError where
  show (PmfError p) = p
