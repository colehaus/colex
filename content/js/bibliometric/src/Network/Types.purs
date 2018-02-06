module Network.Types where

import Control.Monad.Error.Class (class MonadError)
import Data.Array as A
import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe, maybe, fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude

import Math.Probability (Dist, ProbList, extract, runProbList, zipDist)
import Network.Types.Internal as N

type Space = N.Space
type CondPMF = N.CondPMF
type Network = N.Network
type PmfError = N.PmfError

newtype Variable = Variable String
newtype State = State String

data Outcome = Outcome Variable State
outcomeVar :: Outcome -> Variable
outcomeVar (Outcome v _) = v
outcomeState :: Outcome -> State
outcomeState (Outcome _ s) = s

data PMF a b = PMF a (Dist b)
pmfVar :: forall a b. PMF a b -> a
pmfVar (PMF v _) = v
pmfDist :: forall a b. PMF a b -> Dist b
pmfDist (PMF _ m) = m
pmfSpace :: forall a b. PMF a b -> Space a b
pmfSpace (PMF v d) = N.Space v $ extract d

class Spacy a b where
  space :: forall m. MonadError N.PmfError m => Monad m =>
           a -> Array b -> m (Space a b)
instance spacyVarState :: Spacy Variable State where
  space v [] = N.throwPmf $ "No states provided for " <> show v
  space v ss = pure $ N.Space v ss
instance spacyVarsStates :: Spacy (Array Variable) (Array State) where
  space v [] = N.throwPmf $ "No states provided for " <> show v
  space v ss | A.length ss /= A.length (N.combos ss) =
    N.throwPmf $ "Non-exhaustive list of [State] for " <> show v <> ": " <> show ss
             | otherwise = pure $ N.Space v ss

spaceOutcomes :: Space Variable State -> Array Outcome
spaceOutcomes (N.Space v s) = Outcome v <$> s
spaceStates :: forall a b. Space a b -> Array b
spaceStates (N.Space _ s) = s
spaceVar :: forall a b. Space a b -> a
spaceVar (N.Space v _) = v

-- `a` shouldn't depend on itself,
-- i.e. there shouldn't be overlap between `a` and `c`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
condPmf :: forall a b c d m.
           MonadError N.PmfError m => Monad m => Ord d =>
            Show a => Show b => Show c => Show d =>
           Space a b -> Space c d -> (Array ProbList) -> m (CondPMF a b c d)
condPmf xs zs ps |
  A.length (spaceStates zs) /= A.length ps ||
  let l = A.length $ spaceStates xs in
  (not $ F.all (\p -> A.length (runProbList p) == l) ps) =
    N.throwPmf $ "Probabilities don't have same shape as state space: " <>
               show xs <> "\n" <> show zs <> "\n" <> show ps
                | otherwise =
  pure <<< N.CondPMF (spaceVar xs) (spaceVar zs) <<< M.fromFoldable $
  A.zipWith (\z p -> Tuple z $ zipDist (spaceStates xs) p) (spaceStates zs) ps

condMain :: forall a b c d. CondPMF a b c d -> a
condMain (N.CondPMF v _ _) = v
condMainSpace :: forall a b c d. CondPMF a b c d -> Space a b
condMainSpace (N.CondPMF v _ m) = unsafePartial $ N.Space v <<< extract <<< fromJust <<< L.head $ M.values m
condDeps :: forall a b c d. CondPMF a b c d -> c
condDeps (N.CondPMF _ v _) = v
condDepsSpace :: forall a b c d. CondPMF a b c d -> Space c d
condDepsSpace (N.CondPMF _ v m) = N.Space v <<< A.fromFoldable $ M.keys m
condMap :: forall a b c d. CondPMF a b c d -> M.Map d (Dist b)
condMap (N.CondPMF _ _ m) = m

-- Transforms to form expected by Probability and Information modules
functionize :: forall a b c d. Ord d => Partial => CondPMF a b c d -> d -> Dist b
functionize c d = fromJust $ d `M.lookup` condMap c

-- Actually a DAG
-- There shouldn't be any dangling dependencies,
-- i.e. A `c` that doesn't appear in any `a`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
network :: forall a b c d m.
           MonadError N.PmfError m => Monad m => Ord a =>
            Show a => Show b => Show c => Show d =>
           Array (CondPMF a b c d) -> m (Network a b c d)
network cs | A.null cs = N.throwPmf "No CondPMF when constructing Network"
           | N.duplicate $ condMain <$> cs =
  N.throwPmf $ "Duplicated variable when constructing Network: " <> show cs
           | otherwise =
  pure $ N.Network (M.fromFoldable $
  (\(N.CondPMF x z m) -> Tuple x $ N.CondPMFN z m) <$> cs) (condMain <$> cs)

lookup :: forall a b c d. (Ord a) => a -> Network a b c d -> Maybe (CondPMF a b c d)
lookup a (N.Network m _) = N.cp a <$> a `M.lookup` m

netVars :: forall a b c d. Network a b c d -> L.List a
netVars (N.Network m _) = M.keys m
netList :: forall a b c d. Ord a => Network a b c d -> Array (CondPMF a b c d)
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

setNub :: forall a. (Ord a) => Array a -> Array a
setNub = N.setNub

-- Derivable boilerplate

instance eqVariable :: Eq Variable where
  eq (Variable a) (Variable b) = a == b
instance ordVariable :: Ord Variable where
  compare (Variable a) (Variable b) = compare a b
instance showVariable :: Show Variable where
  show (Variable n) = "Variable " <> show n

instance eqState :: Eq State where
  eq (State a) (State b) = a == b
instance ordState :: Ord State where
  compare (State a) (State b) = compare a b
instance showState :: Show State where
  show (State s) = "State " <> show s

instance eqOutcome :: Eq Outcome where
  eq (Outcome va fa) (Outcome vb fb) = va == vb && fa == fb
instance ordOutcome :: Ord Outcome where
  compare (Outcome va fa) (Outcome vb fb) = compare va vb <> compare fa fb
instance showOutcome :: Show Outcome where
  show (Outcome v f) = "Outcome (" <> show v <> ") (" <> show f <> ")"

instance eqPmf :: (Eq a, Eq b) => Eq (PMF a b) where
  eq (PMF va sa) (PMF vb sb) = va == vb && sa == sb
instance ordPmf :: (Ord a, Ord b) => Ord (PMF a b) where
  compare (PMF va sa) (PMF vb sb) = compare va vb <> compare sa sb
instance showPmf :: (Show a, Show b) => Show (PMF a b) where
  show (PMF v s) = "PMF (" <> show v <> ") (" <> show s <> ")"

