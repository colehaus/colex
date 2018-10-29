module Network.Types (module Network.Types, module ForReExport) where

import Control.Monad.Error.Class (class MonadError)
import Data.Array as Array
import Data.Foldable as F
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe, fromJust)
import Data.NonEmpty (fromNonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Prelude

import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Prob.Number (Prob)
import Network.Types.Internal (Space, CondPMF, Network, PmfError)
import Network.Types.Internal (Space, CondPMF, Network, PmfError) as ForReExport
import Network.Types.Internal as N

newtype Variable = Variable String
newtype State = State String

data Outcome = Outcome Variable State
outcomeVar :: Outcome -> Variable
outcomeVar (Outcome v _) = v
outcomeState :: Outcome -> State
outcomeState (Outcome _ s) = s

type Dist' = Dist Prob

data PMF a b = PMF a (Dist' b)
pmfVar :: forall a b. PMF a b -> a
pmfVar (PMF v _) = v
pmfDist :: forall a b. PMF a b -> Dist' b
pmfDist (PMF _ m) = m
pmfSpace :: forall a b. Ord b => PMF a b -> Space a b
pmfSpace (PMF v d) = N.Space v $ extract d
  where
    extract = Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values

class Spacy a b where
  space :: forall m. MonadError N.PmfError m => Monad m =>
           a -> Array b -> m (Space a b)
instance spacyVarState :: Spacy Variable State where
  space v [] = N.throwPmf $ "No states provided for " <> show v
  space v ss = pure $ N.Space v ss
instance spacyVarsStates :: Spacy (Array Variable) (Array State) where
  space v [] = N.throwPmf $ "No states provided for " <> show v
  space v ss | (F.length ss :: Int) /= F.length (N.combos ss) =
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
           MonadError N.PmfError m => Monad m => Ord b => Ord d =>
            Show a => Show b => Show c => Show d =>
           Space a b -> Space c d -> Array (Array Prob) -> m (CondPMF a b c d)
condPmf xs zs ps |
  F.length (spaceStates zs) /= (F.length ps :: Int) ||
  let l :: Int
      l = F.length $ spaceStates xs in
  (not $ F.all (\p -> F.length p == l) ps) =
    N.throwPmf $ "Probabilities don't have same shape as state space: " <>
               show xs <> "\n" <> show zs <> "\n" <> show ps
                | otherwise =
  pure <<< N.CondPMF (spaceVar xs) (spaceVar zs) <<< Map.fromFoldable $
  Array.zipWith (\z p -> Tuple z $ zipDist (spaceStates xs) p) (spaceStates zs) ps
  where
    zipDist keys ps = unsafePartialBecause "Grandfathered" $ Dist.make <<< fromJust <<< nonEmptyMap <<< Map.fromFoldable $ Array.zip keys ps
    nonEmptyMap m =
      (\l -> (Tuple l.key l.value) Indexed.:| (l.key `Map.delete` m)) <$> Map.findMin m




condMain :: forall a b c d. CondPMF a b c d -> a
condMain (N.CondPMF v _ _) = v
condMainSpace :: forall a b c d. Ord b => CondPMF a b c d -> Space a b
condMainSpace (N.CondPMF v _ m) = unsafePartial $ N.Space v <<< extract <<< fromJust <<< List.head $ Map.values m
  where
    extract = Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values

condDeps :: forall a b c d. CondPMF a b c d -> c
condDeps (N.CondPMF _ v _) = v
condDepsSpace :: forall a b c d. CondPMF a b c d -> Space c d
condDepsSpace (N.CondPMF _ v m) = N.Space v <<< Array.fromFoldable $ Map.keys m
condMap :: forall a b c d. CondPMF a b c d -> Map d (Dist' b)
condMap (N.CondPMF _ _ m) = m

-- Transforms to form expected by Probability and Information modules
functionize :: forall a b c d. Ord d => Partial => CondPMF a b c d -> d -> Dist' b
functionize c d = fromJust $ d `Map.lookup` condMap c

-- Actually a DAG
-- There shouldn't be any dangling dependencies,
-- i.e. A `c` that doesn't appear in any `a`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
network :: forall a b c d m.
           MonadError N.PmfError m => Monad m => Ord a =>
            Show a => Show b => Show c => Show d =>
           Array (CondPMF a b c d) -> m (Network a b c d)
network cs | Array.null cs = N.throwPmf "No CondPMF when constructing Network"
           | N.duplicate $ condMain <$> cs =
  N.throwPmf $ "Duplicated variable when constructing Network: " <> show cs
           | otherwise =
  pure $ N.Network (Map.fromFoldable $
  (\(N.CondPMF x z m) -> Tuple x $ N.CondPMFN z m) <$> cs) (condMain <$> cs)

lookup :: forall a b c d. (Ord a) => a -> Network a b c d -> Maybe (CondPMF a b c d)
lookup a (N.Network m _) = N.cp a <$> a `Map.lookup` m

netVars :: forall a b c d. Network a b c d -> List a
netVars (N.Network m _) = Set.toUnfoldable <<< Map.keys $ m
netList :: forall a b c d. Ord a => Network a b c d -> Array (CondPMF a b c d)
netList = N.netList

condPmfMap :: forall a b c d.
              (Dist' b -> Dist' b) -> CondPMF a b c d -> CondPMF a b c d
condPmfMap f (N.CondPMF a c m) = N.CondPMF a c $ f <$> m

netPmfAlter :: forall a b c d. (Ord a) =>
               (CondPMF a b c d -> CondPMF a b c d) -> a ->
               Network a b c d -> Network a b c d
netPmfAlter f a n@(N.Network m as) =
  maybe n (flip N.Network as <<< flip (Map.insert a) m <<< N.cpn <<< f) $ a `lookup` n

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

instance eqPmf :: (Eq a, Ord b) => Eq (PMF a b) where
  eq (PMF va sa) (PMF vb sb) = va == vb && sa == sb
instance ordPmf :: (Ord a, Ord b) => Ord (PMF a b) where
  compare (PMF va sa) (PMF vb sb) = compare va vb <> compare sa sb
instance showPmf :: (Show a, Show b) => Show (PMF a b) where
  show (PMF v s) = "PMF (" <> show v <> ") (" <> show s <> ")"

