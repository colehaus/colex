module Network.Types (
  State(..), Variable(..), PmfError(..),
  -- (~~), (/~), (<~), (>~),
  Outcome(..), outcomeVar, outcomeState,
  PMF(..), mkPmf, pmfVar, pmfDist,
  functionize,
  setNub,
  -- Deps (), deps, runDeps, depsAlter,
  Space (..), space, spaceOutcomes, spaceVar, spaceStates, Spacy,
  CondPMF(), condPmf, condMainSpace, condDepsSpace, condMain, condDeps, condMap,
  Network(), network, netVars, netList, netPmfReshape, lookup) where

import Control.Bind
import Control.Monad.Error
import Control.Monad.Error.Class
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Either
import qualified Data.Foldable as F
import Data.Function (on)
import qualified Data.Traversable as T
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid
import qualified Data.StrMap as SM
import Data.Monoid.All
import qualified Data.Set as S
import Data.Tuple
import Math

import Probability

newtype Variable = Variable String
newtype State = State String

data Outcome = Outcome Variable State
outcomeVar (Outcome v _) = v
outcomeState (Outcome _ s) = s

data PMF a b = PMF a (Dist b)
pmfVar (PMF v _) = v
pmfDist (PMF _ m) = m
pmfSpace (PMF v d) = Space v $ extract d

mkPmf :: forall m. (MonadError PmfError m, Monad m) =>
         String -> [String] -> [Number] -> m (PMF Variable State)
mkPmf v ss ps =
  case T.zipWithA (\a b -> Tuple (State a) <$> prob b) ss ps of
    Just d ->
      case dist d of
        Just p -> pure $ PMF (Variable v) p
        Nothing -> throwError ProbTotal
    Nothing -> throwError ProbBound

-- getVar :: M.Map Deps PMF -> Variable
-- getVar m = pmfVar <<< AU.head $ M.values m

-- newtype Deps = Deps (S.Set Outcome)
-- deps :: forall m. (MonadError PmfError m, Monad m) =>
--         S.Set Outcome -> m Deps
-- deps os | duplicate $ outcomeVar <$> S.toList os = throwError DupDep
-- deps os | otherwise = pure $ Deps os

-- runDeps (Deps d) = d

-- depsAlter :: forall m. (MonadError PmfError m, Monad m) =>
--           ([Outcome] -> [Outcome]) -> Deps -> m Deps
-- depsAlter f = deps <<< S.fromList <<< f <<< S.toList <<< runDeps

data Space a b = Space a [b]
class Spacy a b where
  space :: forall m. (MonadError PmfError m, Monad m) =>
           a -> [b] -> m (Space a b)
instance spacyVarState :: Spacy Variable State where
  space v [] = throwError EmptySpace
  space v ss = pure $ Space v ss
instance spacyVarsStates :: Spacy [Variable] [State] where
  space v [] = throwError EmptySpace
  space v ss | A.length ss == A.length (combos ss) = throwError Exhaust
  space v ss = pure $ Space v ss

spaceOutcomes (Space v s) = Outcome v <$> s
spaceStates (Space _ s) = s
spaceVar (Space v _) = v

-- class Overlap a b where
--   overlap :: a -> b -> Boolean
-- instance overlapListList :: (Ord a) => Overlap [a] [a] where
--   overlap as bs =
--     (A.length <<< S.toList $ (S.fromList as) `S.difference` (S.fromList bs)) /=
--     A.length as
-- instance overlapListEle :: (Ord a) => Overlap [a] a where
--   overlap as a = a `S.member` S.fromList as
-- instance overlapEleList :: (Ord a) => Overlap a [a] where
--   overlap a as = a `S.member` S.fromList as
-- instance overlapEleEle :: (Eq a) => Overlap a a where
--   overlap = (==)
-- instance overlapMismatch :: Overlap a b where
--   overlap _ _ = false

-- `a` shouldn't depend on itself,
-- i.e. there shouldn't be overlap between `a` and `c`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
data CondPMF a b c d = CondPMF a c (M.Map d (Dist b))
condPmf :: forall a b c d m.
           (MonadError PmfError m, Monad m, Ord d) =>
           Space a b -> Space c d -> [ProbList] -> m (CondPMF a b c d)
-- condPmf xs zs _ | spaceVar xs `overlap` spaceVar zs = throwError SelfDep
condPmf xs zs ps |
  A.length (spaceStates zs) /= A.length ps ||
  let l = A.length $ spaceStates xs in
  (not $ F.all (\p -> A.length (runProbList p) == l) ps) =
    throwError CondMismatch
condPmf xs zs ps =
  pure <<< CondPMF (spaceVar xs) (spaceVar zs) <<< M.fromList $
  A.zipWith (\z p -> Tuple z $ zipDist (spaceStates xs) p) (spaceStates zs) ps

condMain (CondPMF v _ _) = v
condMainSpace (CondPMF v _ m) = Space v <<< extract <<< AU.head $ M.values m
condDeps (CondPMF _ v _) = v
condDepsSpace (CondPMF _ v m) = Space v $ M.keys m
condMap (CondPMF _ _ m) = m

-- setMap :: forall a b. (Ord b) => (a -> b) -> S.Set a -> S.Set b
-- setMap f = S.fromList <<< (<$>) f <<< S.toList

combos :: forall a. (Ord a) => [[a]] -> [[a]]
combos = T.sequence <<< (<$>) setNub <<< transpose

-- Assumes all lists are same length
transpose :: forall a. [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = A.map AU.head rows : transpose (A.map AU.tail rows)

-- -- combos :: S.Set Space -> [[State]]
-- -- combos = T.sequence <<< (<$>) spaceStates <<< S.toList

-- condDepSpaces :: CondPMF -> S.Set Space
-- condDepSpaces (CondPMF _ vs m) =
--   S.fromList <<< A.zipWith Space vs <<< (<$>) setNub <<< transpose $ M.keys m

-- depsToSpaces :: S.Set Deps -> S.Set Space
-- depsToSpaces =
--   S.fromList <<< fromJust <<< T.traverse (space <<< S.fromList) <<<
--   A.groupBy ((==) `on` outcomeVar) <<<
--   setNub <<< A.concatMap (S.toList <<< runDeps) <<< S.toList

duplicate :: forall a b. (Ord a) => [a] -> Boolean
duplicate as = A.length as /= A.length (setNub as)

data CondPMFN b c d = CondPMFN c (M.Map d (Dist b))

-- Actually a DAG
-- There shouldn't be any dangling dependencies,
-- i.e. A `c` that doesn't appear in any `a`
-- As far as I can tell, we can't encode this requirement (in full generality)
-- with the type class features available
newtype Network a b c d = Network (M.Map a (CondPMFN b c d))
network :: forall a b c d m. (MonadError PmfError m, Monad m, Ord a) =>
           [CondPMF a b c d] -> m (Network a b c d)
network cs | A.null cs = throwError NoCondPmf
network cs | duplicate $ condMain <$> cs = throwError DupCond
network cs | otherwise =
  pure <<< Network <<< M.fromList $
  (\(CondPMF x z m) -> Tuple x $ CondPMFN z m) <$> cs
-- network cs | let
--   cs' = toList cs
--   deps = A.concatMap condDep cs'
--   vars = S.fromList $ condSpace <$> cs' in
--   not $ F.all (flip S.member vars) deps = throwError NetMismatch

lookup :: forall a b c d. (Ord a) => a -> Network a b c d -> Maybe (CondPMF a b c d)
lookup a (Network m) = (\(CondPMFN c m) -> CondPMF a c m) <$> a `M.lookup` m

-- Transforms to form expected by Probability and Information modules
functionize :: forall a b c d. (Ord d) => CondPMF a b c d -> d -> Dist b
functionize c d = fromJust $ d `M.lookup` condMap c

-- netMap (Network m) = m
netVars (Network m) = M.keys m
netList (Network m) = (\(Tuple a (CondPMFN c m)) -> CondPMF a c m) <$> M.toList m

netPmfReshape :: forall a b c d. (Ord a, Ord d) =>
                 Spread b -> a -> Network a b c d -> Maybe (Network a b c d)
netPmfReshape s a (Network m) =  do
  (CondPMFN c ps) <- a `M.lookup` m
  Network <<< flip (M.insert a) m <<< CondPMFN c <$> T.traverse (reshape s) ps

data PmfError = ProbBound
              | DupDep
              | NoVar Variable
              | BadDist
              | NoPmf
              | Incomp
              | NoCondPmf
              | DupCond
              | ProbParse String
              | ProbTotal
              | NoState Variable
              | Exhaust
              | SelfDep
              | CondMismatch
              | NetMismatch
              | SpaceMismatch
              | EmptySpace
              | OutcomeE Variable State
              | Other String

-- Utility

setNub :: forall a. (Ord a) => [a] -> [a]
setNub = S.toList <<< S.fromList

-- -- Orphan instances

instance semiOrdering :: Semigroup Ordering where
  (<>) LT _ = LT
  (<>) GT _ = GT
  (<>) EQ y = y
instance monoidOrdering :: Monoid Ordering where
  mempty = EQ

instance monadErrMaybe :: MonadError e Maybe where
  throwError = const Nothing
  catchError m _ = m

instance ordSet :: (Ord a) => Ord (S.Set a) where
  compare a b = compare (S.toList a) (S.toList b)

-- instance ordMap :: (Ord k, Ord v) => Ord (M.Map k v) where
--   compare a b = compare (M.toList a) (M.toList b)

-- -- Derivable boilerplate

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

-- instance eqError :: Eq PmfError where
--   (==) ProbBound ProbBound = true
--   (==) DupDep DupDep = true
--   (==) (NoVar a) (NoVar b) = a == b
--   (==) NoPmf NoPmf = true
--   (==) Incomp Incomp = true
--   (==) NoCondPmf NoCondPmf = true
--   (==) DupCond DupCond = true
--   (==) (ProbParse a) (ProbParse b) = a == b
--   (==) (ProbTotal) (ProbTotal) = true
--   (==) (NoState a) (NoState b) = a == b
--   (==) Exhaust Exhaust = true
--   (==) SelfDep SelfDep = true
--   (==) CondMismatch CondMismatch = true
--   (==) NetMismatch NetMismatch = true
--   (==) SpaceMismatch SpaceMismatch = true
--   (==) EmptySpace EmptySpace = true
--   (==) (OutcomeE va sa) (OutcomeE vb sb) = va == vb && sb == sb
--   (==) (Other s) (Other t) = s == t
--   (==) _ _ = false
--   (/=) a b = not $ a == b
instance errPmfError :: Error PmfError where
  noMsg = Other "Unknown error"
  strMsg = Other
instance showPmfError :: Show PmfError where
  show ProbBound = "Probability is outside range [0, 1]"
  show DupDep = "Can't construct dependencies from multiple outcomes " <>
                "for a single variable"
  show (NoVar v) = show v <> " doesn't exist"
  show Incomp = "Incomparable PMFs"
  show NoPmf = "Empty input when constructing conditional PMF"
  show BadDist = "Failed to construct distribution"
  show NoCondPmf = "Empty input when constructing network"
  show DupCond = "Provided duplicate conditional PMFs for single variable"
  show (ProbParse n) = "Can't parse probability " <> show n
  show ProbTotal = "Probability does not equal 1"
  show (NoState v) = "No states listed for " <> show v
  show Exhaust = "Non-exhaustive enumeration of states"
  show SelfDep = "Conditonal PMF depends on itself"
  show CondMismatch = "Probabilites didn't have same shape as state space"
  show NetMismatch = "A conditional PMF depends on a non-existent variable"
  show SpaceMismatch = "Can't construct sample space " <>
                       "from outcomes with different variables"
  show EmptySpace = "Empty input when constructing sample space"
  show (OutcomeE v s) = "Invalid outcome " <> show s <> " specified " <>
                        "for " <> show v
  show (Other s) = s

-- instance eqPmf :: Eq PMF where
--   (==) (PMF va ssa) (PMF vb ssb) = va == vb && ssa == ssb
--   (/=) a b = not $ a == b
-- instance ordPmf :: Ord PMF where
--   compare (PMF va ssa) (PMF vb ssb) = compare va vb <> compare ssa ssb
-- instance showPmf :: Show PMF where
--   show (PMF v ss) = "PMF (" <> show v <> ") (" <> show ss <> ")"

-- -- instance eqDeps :: Eq Deps where
-- --   (==) (Deps a) (Deps b) = a == b
-- --   (/=) a b = not $ a == b
-- -- instance ordDeps :: Ord Deps where
-- --   compare (Deps a) (Deps b) = compare a b
-- -- instance showDeps :: Show Deps where
-- --   show (Deps a) = "Deps " <> show a

-- instance eqSpace :: Eq Space where
--   (==) (Space va psa) (Space vb psb) = va == vb && psa == psb
--   (/=) a b = not $ a == b
-- instance ordSpace :: Ord Space where
--   compare (Space va psa) (Space vb psb) = compare va vb <> compare psa psb
-- instance showSpace :: Show Space where
--   show (Space v ps) = "Space (" <> show v <> ") (" <> show ps <> ")"

-- instance eqCond :: Eq CondPMF where
--   (==) (CondPMF sa vsa ma) (CondPMF sb vsb mb) =
--     sa == sb && vsa == vsb && ma == mb
--   (/=) a b = not $ a == b
-- instance ordCond :: Ord CondPMF where
--   compare (CondPMF sa vsa ma) (CondPMF sb vsb mb) =
--     compare sa sb <> compare vsa vsb <> compare ma mb
instance showCond :: (Show a, Show b, Show c, Show d) =>
         Show (CondPMF a b c d) where
  show (CondPMF s vs m) =
    "CondPMF (" <> show s <> ") (" <> show vs <> ") (" <> show m <> ")"

-- instance eqCondN :: Eq CondPMFN where
--   (==) (CondPMFN sa vsa ma) (CondPMFN sb vsb mb) =
--     sa == sb && vsa == vsb && ma == mb
--   (/=) a b = not $ a == b
-- instance ordCondN :: Ord CondPMFN where
--   compare (CondPMFN sa vsa ma) (CondPMFN sb vsb mb) =
--     compare sa sb <> compare vsa vsb <> compare ma mb
-- instance showCondN :: Show CondPMFN where
--   show (CondPMFN s vs m) =
--     "CondPMF (" <> show s <> ") (" <> show vs <> ") (" <> show m <> ")"


-- instance eqNet :: Eq Network where
--   (==) (Network a) (Network b) = a == b
--   (/=) a b = not $ a == b
-- instance showNet :: Show Network where
--   show (Network m) = "Network " <> show m

