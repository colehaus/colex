module Test.Main where

import Control.Monad
import Control.Bind
import qualified Data.Array as A
import Data.Either
import Data.Either.Unsafe
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Tuple
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Gen hiding (uniform)
import Text.Parsing.Parser (runParserT, ParseError())

import Network
import Network.Parser
import Network.Types hiding (Space(), CondPMF()) -- No type synonym instances
import Network.Types.Internal (Space(), CondPMF()) 
import Probability hiding (choose, oneOf, ProbList(), Prob(), Dist())
import Probability.Internal ((<~), (~~), ProbList(), Prob(), Dist())
import Probability.Information

-- TODO: Finish out properties
main' = do
  trace "H(X) >= 0"
  quickCheck entPos
  trace "H(X|Y) <= H(X)"
  quickCheck condRed
  trace "H(X,Y) <= H(X) + H(Y)"
  quickCheck indepLimit
  trace "H(X,Y) = H(X|Y) + H(Y)"
  quickCheck entChain
  trace "I(X;Y) >= 0"
  quickCheck infoPos
  trace "I(X;Y) = H(X) - H(X|Y)"
  quickCheck infoEnt
  -- trace "I(X;Y|Z) = H(X|Z) - H(X|Y,Z)"
  -- quickCheck condInfoEnt
  -- trace "I(X1,X2;Y) = I(X2;Y|X1) + Y(X1|Y)"
  -- quickCheck infoChain
  trace "D(p(x)||q(x)) >= 0"
  quickCheck divPos
  trace "D(p(x,y)||p(x)p(y)) = I(X;Y)"
  quickCheck divInfo
  -- trace "D(p(x,y)||q(x,y)) = D(p(x)||q(x)) + D(p(y|x)||q(y|x))"
  -- quickCheck divChain
  trace "p(.|a,b,i)||p(.|i) = p(.|a,b,i)||p(.|a,i) + p(.|a,i)||p(.|i) (given that \\alpha is independent of \\beta)"
  quickCheck add
  trace "p(.|a,b,i)||p(.|b,i) = p(.|a,i)||p(.|i) (given that \\alpha is independent of \\beta)"
  quickCheck context

context :: PMFPair Variable State Variable State -> Result
context (PMFPair c (PMF _ as)) =
  d_a'b ~~ d_a <?> show c <> " " <> show as where
    d_a'b = to entropyNum $ divergence (pure unit) (const abs) (const aUbs)
    d_a = to entropyNum $ divergence (pure unit) (const abUs) (const aUbUs)
    abs = joinDists Tuple as bs'a
    aUbs = joinDists Tuple asU bs'a
    abUs = joinDists Tuple as bs'aU
    aUbUs = joinDists Tuple asU bs'aU
    bs'aU d =
      fromJust $ M.lookup d =<< T.traverse (reshape uniform) (condMap c)
    bs'a d = fromJust $ d `M.lookup` condMap c
    asU = fromJust $ reshape uniform as

add :: PMFPair Variable State Variable State -> Result
add (PMFPair c (PMF _ as)) =
  d_ab ~~ d_b'a + d_a <?> show c <> " " <> show as where
    d_ab = to entropyNum $ divergence (pure unit) (const abs) (const aUbUs)
    d_b'a = to entropyNum $ divergence (pure unit) (const abs) (const abUs)
    d_a = to entropyNum $ divergence (pure unit) (const abUs) (const aUbUs)
    abs = joinDists Tuple as bs'a
    aUbUs = joinDists Tuple asU bs'aU
    abUs = joinDists Tuple as bs'aU
    bs'aU d =
      fromJust $ M.lookup d =<< T.traverse (reshape uniform) (condMap c)
    bs'a d = fromJust $ d `M.lookup` condMap c
    asU = fromJust $ reshape uniform as

divInfo :: PMFPair Variable State Variable State -> Result
divInfo (PMFPair c (PMF _ ys)) =
  i_yxs ~~ d_yxs_ysxs <?> show c <> " " <> show ys where
    xs'y = functionize c
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    ysxs = joinDists Tuple ys (const xs)
    i_yxs = to entropyNum $ nonCond mutualInformation yxs fst snd
    d_yxs_ysxs = to entropyNum $ divergence (pure unit) (const yxs) (const ysxs)

divPos :: DivPair Variable State -> Result
divPos (DivPair (PMF _ p) (PMF _ q)) =
  0 <~ to entropyNum (divergence (pure unit) (const p) (const q)) <?>
  show p <> " " <> show q

infoEnt :: PMFPair Variable State Variable State -> Result
infoEnt (PMFPair c (PMF _ ys)) =
  i_yxs ~~ e_xs - e_xs'y <?> show c <> " " <> show ys where
    xs'y = functionize c
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    i_yxs = to entropyNum $ nonCond mutualInformation yxs fst snd
    e_xs = wrapEnt xs
    e_xs'y = to entropyNum $ entropy ys xs'y

entChain :: PMFPair Variable State Variable State -> Result
entChain (PMFPair c (PMF _ ys)) =
  e_xs'y + e_ys ~~ e_xys <?> show c <> " " <> show ys where
    xs'y = functionize c
    e_xs'y = to entropyNum $ entropy ys xs'y
    e_ys = wrapEnt ys
    e_xys = wrapEnt $ joinDists Tuple ys xs'y

condRed :: PMFPair Variable State Variable State -> Result
condRed (PMFPair c (PMF _ ys)) =
  e_xs'y <~ e_xs <?> show c <> " " <> show ys where
    xs'y = functionize c
    e_xs'y = to entropyNum $ entropy ys xs'y
    xs = marginalize snd $ joinDists Tuple ys xs'y
    e_xs = wrapEnt xs

indepLimit :: PMFPair Variable State Variable State -> Result
indepLimit (PMFPair c (PMF _ ys)) =
  e_yxs <~ e_xs + e_ys <?> show c <> " " <> show ys where
    xs'y = functionize c
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    e_yxs = wrapEnt yxs
    e_xs = wrapEnt xs
    e_ys = wrapEnt ys

entPos :: PMF State Variable -> Result
entPos (PMF _ d) =
  0 <~ wrapEnt d <?> show d

infoPos :: PMFPair Variable State Variable State -> Result
infoPos (PMFPair c (PMF _ ys)) =
  0 <~ i_yxs <?> show c <> " " <> show ys where
    xs'y = functionize c
    yxs = joinDists Tuple ys xs'y
    i_yxs = to entropyNum $ nonCond mutualInformation yxs fst snd

wrapEnt :: forall a. (Eq a) => Dist a -> Number
wrapEnt = to entropyNum <<< nonCond entropy

instance arbProb :: Arbitrary Prob where
  arbitrary = do
    p <- prob <$> choose 0 1
    maybe arbitrary pure $ p
instance arbState :: Arbitrary State where
  arbitrary = State <$> arbitrary
instance arbVariable :: Arbitrary Variable where
  arbitrary = Variable <$> arbitrary
instance arbDist :: (Arbitrary a) => Arbitrary (Dist a) where
  arbitrary = do
    p <- fromFreqs <$> arbitrary
    maybe arbitrary pure $ p
instance arbSpaceSingle :: Arbitrary (Space Variable State) where
  arbitrary = do
    s <- space <$> arbitrary <*> arbitrary
    maybe arbitrary pure $ s
instance arbSpaceMulti :: Arbitrary (Space [Variable] [State]) where
  arbitrary = do
    spaces <- arrayOf arbitrary
    Tuple vs ss <- pure <<< unzip $ (\s -> Tuple (spaceVar s) $ spaceStates s) <$> spaces
    let s = space vs (T.sequence ss)
    maybe arbitrary pure $ s
instance arbPmf :: (Arbitrary a, Arbitrary b) => Arbitrary (PMF a b) where
  arbitrary = PMF <$> arbitrary <*> arbitrary
instance arbCondSingle :: Arbitrary (CondPMF Variable State Variable State) where
  arbitrary = do
    _ <- pure unit
    arbCond (==)
instance arbCondMulti :: Arbitrary (CondPMF Variable State [Variable] [State]) where
  arbitrary = do
    _ <- pure unit
    arbCond (\m d -> m `S.member` S.fromList d)
arbCond :: forall a b c d. 
       (Arbitrary (Space a b), Arbitrary (Space c d), Arbitrary (CondPMF a b c d),
        Show a, Show b, Show c, Show d, Ord d) =>
       (a -> c -> Boolean) -> Gen (CondPMF a b c d)
arbCond f = do
    mainSpace <- arbitrary
    depSpace <- arbitrary
    if f (spaceVar mainSpace) (spaceVar depSpace)
      then arbitrary
      else do
        probLists <- vectorOf (A.length $ spaceStates depSpace)
                              (probListArb <<< A.length $ spaceStates mainSpace)
        let c = condPmf mainSpace depSpace probLists
        maybe arbitrary pure $ c
data PMFPair a b c d = PMFPair (CondPMF a b c d) (PMF c d)
instance arbPMFPair :: (Arbitrary (CondPMF a b c d)) =>
         Arbitrary (PMFPair a b c d) where
  arbitrary = do
    condPmf <- arbitrary
    pmf <- pairedPmf condPmf
    pure $ PMFPair condPmf pmf
data DivPair a b = DivPair (PMF a b) (PMF a b)
instance arbDivPair :: (Arbitrary (PMF a b), Arbitrary a) =>
         Arbitrary (DivPair a b) where
  arbitrary = do
    pmf@(PMF _ d') <- arbitrary
    let states = extract d'
    d <- zipDist states <$> (probListArb $ A.length states)
    v <- arbitrary
    pure <<< DivPair pmf $ PMF v d

pairedPmf :: forall a b c d. CondPMF a b c d -> Gen (PMF c d)
pairedPmf c = do
  let space = condDepsSpace c
  let states = spaceStates space
  PMF (spaceVar space) <<< zipDist states <$> probListArb (A.length states)

normalize :: [Number] -> [Number]
normalize ns = let s = F.sum ns in flip (/) s <$> ns
probListArb :: Number -> Gen ProbList
probListArb n =
  fromJust <<< (probList <=< T.traverse prob) <<< normalize <$>
  vectorOf n (choose 0 1)

main = do
  print $ gain' ["a", "b", "c", "d"] [] input''
  let g1 = netScores input''
  print g1
  print <<< F.sum $ (to entropyNum <<< snd) <$> S.toList g1

gain' l r = to entropyNum <<< gain (Variable <$> l) (Variable <$> r)

input'' = fromRight $ fromRight input'
input' :: Either PmfError (Either ParseError
                           (Network Variable State [Variable] [State]))
input' = runParserT netString networkP

netString = """
----
b c | d P
----
t t | t 0.9
    | f 0.1
t f | t 0.85
    | f 0.15
f t | t 0.825
    | f 0.175
f f | t 0.8
    | f 0.2

----
a | c P
----
t | t 0.75
  | f 0.25
f | t 0.7
  | f 0.3

----
a | b P
----
t | t 0.65
  | f 0.35
f | t 0.6
  | f 0.4

----
| a P
----
| t 0.55
| f 0.45
"""
