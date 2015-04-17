module Network.Types where

import Control.Bind
import Control.Monad.Error.Class
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Tuple

import Math.Probability
import qualified Network.Types.Internal as N

type Space = N.Space
type CondPMF = N.CondPMF
type Network = N.Network
type PmfError = N.PmfError

newtype Variable = Variable String
newtype State = State String

data Outcome = Outcome Variable State
outcomeVar (Outcome v _) = v
outcomeState (Outcome _ s) = s

data PMF a b = PMF a (Dist b)
pmfVar (PMF v _) = v
pmfDist (PMF _ m) = m
pmfSpace (PMF v d) = N.Space v $ extract d

class Spacy a b where
  space :: forall m. (MonadError N.PmfError m, Monad m) =>
           a -> [b] -> m (Space a b)
instance spacyVarState :: Spacy Variable State where
  space v [] = N.throwPmf $ "No states provided for " <> show v
  space v ss = pure $ N.Space v ss
instance spacyVarsStates :: Spacy [Variable] [State] where
  space v [] = N.throwPmf $ "No states provided for " <> show v
  space v ss | A.length ss /= A.length (N.combos ss) =
    N.throwPmf $ "Non-exhaustive list of [State] for " <> show v <> ": " <> show ss
             | otherwise = pure $ N.Space v ss

spaceOutcomes (N.Space v s) = Outcome v <$> s
spaceStates (N.Space _ s) = s
spaceVar (N.Space v _) = v

-- `a` shouldn't depend on itself,
-- i.e. there shouldn't be overlap between `a` and `c`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
condPmf :: forall a b c d m.
           (MonadError N.PmfError m, Monad m, Ord d,
            Show a, Show b, Show c, Show d) =>
           Space a b -> Space c d -> [ProbList] -> m (CondPMF a b c d)
condPmf xs zs ps |
  A.length (spaceStates zs) /= A.length ps ||
  let l = A.length $ spaceStates xs in
  (not $ F.all (\p -> A.length (runProbList p) == l) ps) =
    N.throwPmf $ "Probabilities don't have same shape as state space: " <>
               show xs <> "\n" <> show zs <> "\n" <> show ps
                | otherwise =
  pure <<< N.CondPMF (spaceVar xs) (spaceVar zs) <<< M.fromList $
  A.zipWith (\z p -> Tuple z $ zipDist (spaceStates xs) p) (spaceStates zs) ps

condMain (N.CondPMF v _ _) = v
condMainSpace (N.CondPMF v _ m) = N.Space v <<< extract <<< AU.head $ M.values m
condDeps (N.CondPMF _ v _) = v
condDepsSpace (N.CondPMF _ v m) = N.Space v $ M.keys m
condMap (N.CondPMF _ _ m) = m

-- Transforms to form expected by Probability and Information modules
functionize :: forall a b c d. (Ord d) => CondPMF a b c d -> d -> Dist b
functionize c d = fromJust $ d `M.lookup` condMap c

-- Actually a DAG
-- There shouldn't be any dangling dependencies,
-- i.e. A `c` that doesn't appear in any `a`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
network :: forall a b c d m.
           (MonadError N.PmfError m, Monad m, Ord a,
            Show a, Show b, Show c, Show d) =>
           [CondPMF a b c d] -> m (Network a b c d)
network cs | A.null cs = N.throwPmf "No CondPMF when constructing Network"
           | N.duplicate $ condMain <$> cs =
  N.throwPmf $ "Duplicated variable when constructing Network: " <> show cs
           | otherwise =
  pure $ N.Network (M.fromList $
  (\(N.CondPMF x z m) -> Tuple x $ N.CondPMFN z m) <$> cs) (condMain <$> cs)

lookup :: forall a b c d. (Ord a) => a -> Network a b c d -> Maybe (CondPMF a b c d)
lookup a (N.Network m _) = N.cp a <$> a `M.lookup` m

netVars (N.Network m _) = M.keys m
netList = N.netList

condPmfMap :: forall a b c d.
              (Dist b -> Dist b) -> CondPMF a b c d -> CondPMF a b c d
condPmfMap f (N.CondPMF a c m) = N.CondPMF a c $ f <$> m

netPmfAlter :: forall a b c d. (Ord a) =>
               (CondPMF a b c d -> CondPMF a b c d) -> a ->
               Network a b c d -> Network a b c d
netPmfAlter f a n@(N.Network m as) =
  maybe n (flip N.Network as <<< flip (M.insert a) m <<< N.cpn <<< f) $ a `lookup` n

-- Utility

setNub :: forall a. (Ord a) => [a] -> [a]
setNub = N.setNub

-- Derivable boilerplate

instance eqVariable :: Eq Variable where
  (==) (Variable a) (Variable b) = a == b
  (/=) a b = not $ a == b
instance ordVariable :: Ord Variable where
  compare (Variable a) (Variable b) = compare a b
instance showVariable :: Show Variable where
  show (Variable n) = "Variable " <> show n

instance eqState :: Eq State where
  (==) (State a) (State b) = a == b
  (/=) a b = not $ a == b
instance ordState :: Ord State where
  compare (State a) (State b) = compare a b
instance showState :: Show State where
  show (State s) = "State " <> show s

instance eqOutcome :: Eq Outcome where
  (==) (Outcome va fa) (Outcome vb fb) = va == vb && fa == fb
  (/=) a b = not $ a == b
instance ordOutcome :: Ord Outcome where
  compare (Outcome va fa) (Outcome vb fb) = compare va vb <> compare fa fb
instance showOutcome :: Show Outcome where
  show (Outcome v f) = "Outcome (" <> show v <> ") (" <> show f <> ")"

instance eqPmf :: (Eq a, Eq b) => Eq (PMF a b) where
  (==) (PMF va sa) (PMF vb sb) = va == vb && sa == sb
  (/=) a b = not $ a == b
instance ordPmf :: (Ord a, Ord b) => Ord (PMF a b) where
  compare (PMF va sa) (PMF vb sb) = compare va vb <> compare sa sb
instance showPmf :: (Show a, Show b) => Show (PMF a b) where
  show (PMF v s) = "PMF (" <> show v <> ") (" <> show s <> ")"

