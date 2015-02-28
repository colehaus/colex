module Information.Types
       (State(..), Variable(..), (~~), (<~), (>~), PmfError(..),
        Prob(), prob, runProb, Entropy (..), runEntropy,
        Outcome(..), outcomeVar, outcomeState,
        PMF(), PMFBody(), pmf, mkPmf, pmfVar, pmfBody,
        Deps (), deps, runDeps, depsAlter,
        Space (..), space, spaceOutcomes,
        CondPMF(), CondPMFBody(), condPmf, condVar, condBody,
        condDepSpaces, condMainSpace, depsToSpaces, combos, depVars,
        Network(), network, runNet, netPmfAlter,
        on) where
 

import Control.Bind
import Control.Monad.Error
import Control.Monad.Error.Class
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Either
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid
import qualified Data.StrMap as SM
import Data.Monoid.All
import qualified Data.Set as S
import Data.Tuple

newtype Entropy = Entropy Number
runEntropy (Entropy n) = n
newtype Variable = Variable String
newtype State = State String

newtype Prob = Prob Number
prob :: forall m. (MonadError PmfError m, Monad m) => Number -> m Prob
prob n | 0 <~ n && n <~ 1 = return $ Prob n
prob n | otherwise = throwError $ ProbBound n

runProb (Prob p) = p

data Outcome = Outcome Variable State
outcomeVar (Outcome v _) = v
outcomeState (Outcome _ s) = s

type PMFBody = M.Map State Prob
data PMF = PMF Variable PMFBody
pmf :: forall m. (MonadError PmfError m, Monad m) =>
       Variable -> PMFBody -> m PMF
pmf v m | M.size m == 0 = throwError $ NoState v
pmf v m | not (probTotal $ M.values m) =
    throwError $ ProbTotal v
pmf v m | otherwise = return $ PMF v m

probTotal :: [Prob] -> Boolean
probTotal ps = F.sum (runProb <$> ps) ~~ 1

pmfVar (PMF v _) = v
pmfBody (PMF _ m) = m

mkPmf :: forall m. (MonadError PmfError m, Monad m) =>
         String -> [String] -> [Number] -> m PMF
mkPmf v ss ps =
  pmf (Variable v) =<< M.fromList <$>
  T.zipWithA (\a b -> Tuple (State a) <$> prob b) ss ps

all :: forall a. (a -> a -> Boolean) -> [a] -> Boolean
all f [] = true
all f as = 
  runAll <<< F.mconcat <<< A.zipWith (\a b -> All $ f a b) as $ AU.tail as

getVar :: M.Map Deps PMF -> Variable
getVar m = pmfVar <<< AU.head $ M.values m

newtype Deps = Deps (S.Set Outcome)
deps :: forall m. (MonadError PmfError m, Monad m) =>
        S.Set Outcome -> m Deps
deps os | duplicate $ outcomeVar <$> S.toList os = throwError DupDep
deps os | otherwise = return $ Deps os

runDeps (Deps d) = d

depsAlter :: forall m. (MonadError PmfError m, Monad m) =>
          ([Outcome] -> [Outcome]) -> Deps -> m Deps
depsAlter f = deps <<< S.fromList <<< f <<< S.toList <<< runDeps

data Space = Space Variable (S.Set State)
space :: forall m. (MonadError PmfError m, Monad m) =>
         S.Set Outcome -> m Space
space os | S.isEmpty os = throwError EmptySpace
space os | not $ all ((==) `on` outcomeVar) $ S.toList os =
  throwError SpaceMismatch
space os = 
  return <<< Space (outcomeVar <<< AU.head $ S.toList os) $
  setMap outcomeState os

spaceOutcomes :: Space -> S.Set Outcome
spaceOutcomes (Space v s) = setMap (Outcome v) s

type CondPMFBody = M.Map Deps PMFBody

data CondPMF = CondPMF Variable CondPMFBody
condPmf :: forall m. (MonadError PmfError m, Monad m) =>
                  (M.Map Deps PMF) -> m CondPMF
condPmf ps | M.isEmpty ps = throwError NoPmf
condPmf ps | not <<< all (==) $ pmfVar <$> M.values ps =
  throwError  <<< CondMismatch $ getVar ps
condPmf ps |
  not <<< all (==) $ S.fromList <<< M.keys <<< pmfBody <$> M.values ps =
    throwError  <<< CondMismatch $ getVar ps
condPmf ps | 
  getVar ps `F.elem`
  A.concatMap ((<$>) outcomeVar <<< S.toList <<< runDeps) (M.keys ps) =
    throwError <<< SelfDep $ getVar ps
condPmf ps |
  S.fromList (M.keys ps) /= (combos <<< depsToSpaces <<< S.fromList $ M.keys ps) =
    throwError <<< Exhaust $ getVar ps
condPmf ps = return <<< CondPMF (getVar ps) $ M.map pmfBody ps

condVar (CondPMF v _) = v
condBody (CondPMF _ m) = m

setMap :: forall a b. (Ord b) => (a -> b) -> S.Set a -> S.Set b
setMap f = S.fromList <<< (<$>) f <<< S.toList

combos :: S.Set Space -> S.Set Deps
combos =
  S.fromList <<< (<$>) (Deps <<< S.fromList) <<< T.sequence <<<
  (<$>) (S.toList <<< spaceOutcomes) <<< S.toList

depsToSpaces :: S.Set Deps -> S.Set Space
depsToSpaces =
  S.fromList <<< fromJust <<< T.traverse (space <<< S.fromList) <<<
  A.groupBy ((==) `on` outcomeVar) <<<
  setNub <<< A.concatMap (S.toList <<< runDeps) <<< S.toList

depVars :: CondPMFBody -> S.Set Variable
depVars = setMap outcomeVar <<< runDeps <<< AU.head <<< M.keys

condMainSpace :: Variable -> CondPMFBody -> Space
condMainSpace v = Space v <<< S.fromList <<< M.keys <<< AU.head <<< M.values

condDepSpaces :: CondPMFBody -> S.Set Space
condDepSpaces = depsToSpaces <<< S.fromList <<< M.keys

duplicate :: forall a b. (Ord a) => [a] -> Boolean
duplicate as = A.length as /= A.length (setNub as)

newtype Network = Network (M.Map Variable CondPMFBody)
network :: forall m. (MonadError PmfError m, Monad m) =>
           S.Set CondPMF -> m Network
network cs | S.isEmpty cs = throwError NoCondPmf 
network cs | duplicate $ condVar <$> S.toList cs = 
  throwError DupCond
network cs | let
  deps = A.concatMap (S.toList <<< condDepSpaces <<< condBody) $ S.toList cs
  vars =
    S.fromList $ (\c -> condMainSpace (condVar c) $ condBody c) <$>
    S.toList cs in
  not <<< runAll <<< F.mconcat $ All <<< flip S.member vars <$> deps =
     throwError NetMismatch
network cs | otherwise = 
  return <<< Network <<< M.fromList $
  (\(CondPMF v m) -> Tuple v m) <$> S.toList cs

runNet (Network m) = m

netPmfAlter :: forall m. (MonadError PmfError m, Monad m) =>
            (PMFBody -> [Prob]) -> Variable -> Network -> m Network
netPmfAlter f v (Network m) =
  maybe (throwError $ NoVar v) h $ M.lookup v m where
    h c = Network <<< flip (M.insert v) m <$> T.traverse g c
    g p = let p' = f p in
      if not $ probTotal p'
      then throwError $ ProbTotal v
      else return <<< M.fromList $ zip (M.keys p) p'

data PmfError = ProbBound Number
              | DupDep
              | NoVar Variable
              | NoPmf
              | NoCondPmf
              | DupCond
              | ProbParse String
              | ProbTotal Variable
              | NoState Variable
              | Exhaust Variable
              | SelfDep Variable
              | CondMismatch Variable
              | NetMismatch
              | SpaceMismatch
              | EmptySpace
              | OutcomeE Variable State
              | Other String


-- Utility

setNub :: forall a. (Ord a) => [a] -> [a]
setNub = S.toList <<< S.fromList

on :: forall a b c. (b -> b -> c)-> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b)

epsilon :: Number
epsilon = 0.0005

infix 4 ~~
(~~) :: Number -> Number -> Boolean
(~~) a b = b - epsilon <= a && a <= b + epsilon
(/~) :: Number -> Number -> Boolean
(/~) a b = not $ a ~~ b

infix 4 <~
(<~) :: Number -> Number -> Boolean
(<~) a b = a <= b + epsilon

infix 4 >~
(>~) :: Number -> Number -> Boolean
(>~) a b = a >= b - epsilon


-- Orphan instances

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

instance ordMap :: (Ord k, Ord v) => Ord (M.Map k v) where
  compare a b = compare (M.toList a) (M.toList b)

-- Derivable boilerplate

instance eqEnt :: Eq Entropy where
  (==) (Entropy a) (Entropy b) = a == b
  (/=) a b = not $ a == b
instance ordEnt :: Ord Entropy where
  compare (Entropy a) (Entropy b) = compare a b
instance showEnt :: Show Entropy where
  show (Entropy n) = "Entropy " <> show n

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

instance eqProb :: Eq Prob where
  (==) (Prob a) (Prob b) = a == b
  (/=) a b = not $ a == b
instance ordProb :: Ord Prob where
  compare (Prob a) (Prob b) = compare a b
instance showProb :: Show Prob where
  show (Prob a) = show a

instance eqOutcome :: Eq Outcome where
  (==) (Outcome va fa) (Outcome vb fb) = va == vb && fa == fb
  (/=) a b = not $ a == b
instance ordOutcome :: Ord Outcome where
  compare (Outcome va fa) (Outcome vb fb) = compare va vb <> compare fa fb
instance showOutcome :: Show Outcome where
  show (Outcome v f) = "Outcome (" <> show v <> ") (" <> show f <> ")"

instance eqError :: Eq PmfError where
  (==) (ProbBound a) (ProbBound b) = a == b
  (==) DupDep DupDep = true
  (==) (NoVar a) (NoVar b) = a == b
  (==) NoPmf NoPmf = true
  (==) NoCondPmf NoCondPmf = true
  (==) DupCond DupCond = true
  (==) (ProbParse a) (ProbParse b) = a == b
  (==) (ProbTotal a) (ProbTotal b) = a == b
  (==) (NoState a) (NoState b) = a == b
  (==) (Exhaust a) (Exhaust b) = a == b
  (==) (SelfDep a) (SelfDep b) = a == b
  (==) (CondMismatch a) (CondMismatch b) = a == b
  (==) NetMismatch NetMismatch = true
  (==) SpaceMismatch SpaceMismatch = true
  (==) EmptySpace EmptySpace = true
  (==) (OutcomeE va sa) (OutcomeE vb sb) = va == vb && sb == sb
  (==) (Other s) (Other t) = s == t
  (==) _ _ = false
  (/=) a b = not $ a == b
instance errPmfError :: Error PmfError where
  noMsg = Other "Unknown error"
  strMsg = Other
instance showPmfError :: Show PmfError where
  show (ProbBound n) = "Probability " <> show n <> " is outside range [0, 1]"
  show DupDep = "Can't construct dependencies from multiple outcomes " <>
                "for a single variable"
  show (NoVar v) = show v <> " doesn't exist"
  show NoPmf = "Empty input when constructing conditional PMF"
  show NoCondPmf = "Empty input when constructing network"
  show DupCond = "Provided duplicate conditional PMFs for single variable"
  show (ProbParse n) = "Can't parse probability " <> show n
  show (ProbTotal v) = "Probability does not equal 1 for " <> show v
  show (NoState v) = "No states listed for " <> show v
  show (Exhaust v) = show v <> " doesn't have a PMF  " <>
                  "for all combinations of dependency states"
  show (SelfDep v) = show v <> " depends on itself"
  show (CondMismatch v) = "Provided PMF of the wrong " <> show v <>
                          " when constructing conditional PMF"
  show NetMismatch = "A conditional PMF depends on a non-existent variable"
  show SpaceMismatch = "Can't construct sample space " <>
                       "from outcomes with different variables"
  show EmptySpace = "Empty input when constructing sample space"
  show (OutcomeE v s) = "Invalid outcome " <> show s <> " specified " <>
                        "for " <> show v
  show (Other s) = s

instance eqPmf :: Eq PMF where
  (==) (PMF va ssa) (PMF vb ssb) = va == vb && ssa == ssb
  (/=) a b = not $ a == b
instance ordPmf :: Ord PMF where
  compare (PMF va ssa) (PMF vb ssb) = compare va vb <> compare ssa ssb
instance showPmf :: Show PMF where
  show (PMF v ss) = "PMF (" <> show v <> ") (" <> show ss <> ")"

instance eqDeps :: Eq Deps where
  (==) (Deps a) (Deps b) = a == b
  (/=) a b = not $ a == b
instance ordDeps :: Ord Deps where
  compare (Deps a) (Deps b) = compare a b
instance showDeps :: Show Deps where
  show (Deps a) = "Deps " <> show a

instance eqSpace :: Eq Space where
  (==) (Space va psa) (Space vb psb) = va == vb && psa == psb
  (/=) a b = not $ a == b
instance ordSpace :: Ord Space where
  compare (Space va psa) (Space vb psb) = compare va vb <> compare psa psb
instance showSpace :: Show Space where
  show (Space v ps) = "Space (" <> show v <> ") (" <> show ps <> ")"

instance eqCond :: Eq CondPMF where
  (==) (CondPMF va psa) (CondPMF vb psb) = va == vb && psa == psb
  (/=) a b = not $ a == b
instance ordCond :: Ord CondPMF where
  compare (CondPMF va psa) (CondPMF vb psb) = compare va vb <> compare psa psb
instance showCond :: Show CondPMF where
  show (CondPMF v ps) = "CondPMF (" <> show v <> ") (" <> show ps <> ")"

instance eqNet :: Eq Network where
  (==) (Network a) (Network b) = a == b
  (/=) a b = not $ a == b
instance showNet :: Show Network where
  show (Network m) = "Network " <> show m

