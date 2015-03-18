module Test.Main where

import Control.Arrow
import Control.Bind
import qualified Data.Array as A
import Data.Either
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Tuple
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Gen hiding (uniform)
import qualified Test.Unit as U
import Text.Parsing.Parser

import Information
import Network
import Network.Parser
import Network.Types
import Probability hiding (choose, oneOf)

main = do
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
  quickCheck cite
main' = do
  -- let f2 = gain' ["a", "b", "c", "d"] ["a", "b", "c"] input''
  -- let f6 = gain' ["c", "d"] ["c"] input''
  -- let f7 = gain' ["b", "d"] ["b"] input''
  -- let f8 = gain' ["d"] [] input''
  -- let f9 = gain' ["a", "d"] ["a"] input''
  -- let f3 = gain' ["a", "b"] ["a"] input''
  -- let f4 = gain' ["a", "c"] ["a"] input''
  -- let f5 = gain' ["a"] [] input''
  print $ gain' ["a", "b", "c", "d"] [] input'' 
  let g1 = netScores input''
  print g1
  print <<< F.sum $ (to entropyNum <<< snd) <$> S.toList g1
  -- ABCD||ABC = ABCD||ABC - D|| + D||
  -- ABCD||ABC = (BD||B - D||) + (CD||C - D||) +
  --             (ABCD||ABC - (BD||B - D||) - (CD||C - D||))
  -- let f = [f2, f3, f4, f5]
  -- print f1
  -- print f
  -- print <<< F.sum $ to entropyNum <$> f

  -- print $ divergence (pure unit) (const dist2AB) (const dist2A)
  -- print $ divergence (pure unit) (const dist2B) (const dist2)
  -- print $ divergence (pure unit) (const dist2AB) (const dist2B)
  -- print $ divergence (pure unit) (const dist2A) (const dist2)
  -- print $ divergence (pure unit) (const dist2AB) (const dist2)
  -- print $ divergence (pure unit) (const dist3) (const dist3A)
  -- print $ divergence (pure unit) (const dist3) (const dist3B)
  -- print $ divergence (pure unit) (const dist3) (const dist3C)
  -- print $ divergence (pure unit) (const dist2) (const dist2A)
  -- print $ divergence (pure unit) (const dist2) (const dist2B)
  -- print <<< netDivergence <<< fromRight $ fromRight input'

-- p(.|a,b,i)||p(.|i) = p(.|a,b,i)||p(.|a,i) + p(.|a,i)||p(.|i)
-- D(A,B) = D(B|A) + D(A)

filterM :: forall a m. (Monad m) => (a -> m Boolean) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  b <- p x
  xs' <- filterM p xs
  return $ if b
           then x : xs'
           else xs'

-- (ABCD||) = (ABCD||ABC) + (AB||A) + (AC||A) + (A||) =
-- (ABCD||ABC) - (D||) + (D||) +
-- (AB||A) - (B||) + (B||) +
-- (AC||A) - (C||) + (C||) +
-- (A||) - (A||) + (A||)

-- (A||) + (AC||A) - (C||) + (AB||A) - (B||)
-- (B||) + 1/2((ABCD||ABC)-(D||))
-- (C||) + 1/2((ABCD||ABC)-(D||))
-- (D||)

powerset :: forall a. [a] -> [[a]]
powerset = filterM (const [true, false])

subset :: forall a. (Ord a) => S.Set a -> S.Set a -> Boolean
subset a b = S.isEmpty $ a `S.difference` b
properSubset :: forall a. (Ord a) => S.Set a -> S.Set a -> Boolean
properSubset a b = subset a b && (not <<< S.isEmpty $ b `S.difference` a)

divPairs :: forall a. (Ord a) => [a] -> [Tuple [a] [a]]
divPairs as =
  A.filter (uncurry $ properSubset `on` S.fromList) $ Tuple <$> p <*> p where
    p = powerset as

gain' l r = to entropyNum <<< gain (Variable <$> l) (Variable <$> r)

-- sumTo :: Network Variable State [Variable] [State] ->
--          [[Tuple [Variable] [Variable]]]
-- sumTo n =
--   (<$>) fst <<< A.filter ((~~) fullGain <<< snd) $
--   (\ts -> Tuple ((nonUniform *** nonUniform) <$> ts) <<< F.sum $ gain <$> ts) <$> combos where
--     -- This form's a bit more readable
--     nonUniform vs' = S.toList $ S.fromList vs `S.difference` S.fromList vs'
--     gain =
--       to entropyNum <<< uncurry
--       (divergence (pure unit) `on` (const <<< netDist <<< flip uniformize n))
--     combos = powerset $ divPairs vs
--     fullGain = gain $ Tuple [] vs
--     vs = M.keys $ netMap n



input'' :: Network Variable State [Variable] [State]
input'' = fromRight $ fromRight input'
input' :: Either PmfError (Either ParseError
                           (Network Variable State [Variable] [State]))
input' = runParserT netString networkP
fromRight (Right a) = a
fromLeft (Left a) = a

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


-- f :: [Tuple String Number] -> Dist String
-- f = fromJust <<< (dist <=< T.traverse (\(Tuple s n) -> Tuple s <$> prob n))

-- dist2AB = f [
--   Tuple "11" $ 0.8 * 0.9,
--   Tuple "12" $ 0.2 * 0.9,
--   Tuple "21" $ 0.5 * 0.1,
--   Tuple "22" $ 0.5 * 0.1
-- ]
-- dist2A = f [
--   Tuple "11" $ 0.8 * 0.5,
--   Tuple "12" $ 0.2 * 0.5,
--   Tuple "21" $ 0.5 * 0.5,
--   Tuple "22" $ 0.5 * 0.5
-- ]
-- dist2B = f [
--   Tuple "11" $ 0.5 * 0.9,
--   Tuple "12" $ 0.5 * 0.9,
--   Tuple "21" $ 0.5 * 0.1,
--   Tuple "22" $ 0.5 * 0.1
-- ]
-- dist2 = f [
--   Tuple "11" $ 0.5 * 0.5,
--   Tuple "12" $ 0.5 * 0.5,
--   Tuple "21" $ 0.5 * 0.5,
--   Tuple "22" $ 0.5 * 0.5
-- ]


-- dist2 = f [
--   Tuple ["1", "1"] $ 0.2 * 0.4,
--   Tuple ["1", "2"] $ 0.2 * 0.6,
--   Tuple ["2", "1"] $ 0.8 * 0.4,
--   Tuple ["2", "2"] $ 0.8 * 0.6
--   ]
-- dist2A = f [
--   Tuple ["1", "1"] $ 0.5 * 0.4,
--   Tuple ["1", "2"] $ 0.5 * 0.6,
--   Tuple ["2", "1"] $ 0.5 * 0.4,
--   Tuple ["2", "2"] $ 0.5 * 0.6
--   ]
-- dist2B = f [
--   Tuple ["1", "1"] $ 0.2 * 0.5,
--   Tuple ["1", "2"] $ 0.2 * 0.5,
--   Tuple ["2", "1"] $ 0.8 * 0.5,
--   Tuple ["2", "2"] $ 0.8 * 0.5
--   ]

-- dist3 = f [
--   Tuple ["1", "1", "t"] $ 0.2 * 0.4 * 0.4,
--   Tuple ["1", "1", "f"] $ 0.2 * 0.4 * 0.6,
--   Tuple ["1", "2", "t"] $ 0.2 * 0.6 * 0.6,
--   Tuple ["1", "2", "f"] $ 0.2 * 0.6 * 0.4,
--   Tuple ["2", "1", "t"] $ 0.8 * 0.4 * 0.6,
--   Tuple ["2", "1", "f"] $ 0.8 * 0.4 * 0.4,
--   Tuple ["2", "2", "t"] $ 0.8 * 0.6 * 0.7,
--   Tuple ["2", "2", "f"] $ 0.8 * 0.6 * 0.3
--   ]

-- dist3A = f [
--   Tuple ["1", "1", "t"] $ 0.5 * 0.4 * 0.4,
--   Tuple ["1", "1", "f"] $ 0.5 * 0.4 * 0.6,
--   Tuple ["1", "2", "t"] $ 0.5 * 0.6 * 0.6,
--   Tuple ["1", "2", "f"] $ 0.5 * 0.6 * 0.4,
--   Tuple ["2", "1", "t"] $ 0.5 * 0.4 * 0.6,
--   Tuple ["2", "1", "f"] $ 0.5 * 0.4 * 0.4,
--   Tuple ["2", "2", "t"] $ 0.5 * 0.6 * 0.7,
--   Tuple ["2", "2", "f"] $ 0.5 * 0.6 * 0.3
--   ]

-- dist3B = f [
--   Tuple ["1", "1", "t"] $ 0.2 * 0.5 * 0.4,
--   Tuple ["1", "1", "f"] $ 0.2 * 0.5 * 0.6,
--   Tuple ["1", "2", "t"] $ 0.2 * 0.5 * 0.6,
--   Tuple ["1", "2", "f"] $ 0.2 * 0.5 * 0.4,
--   Tuple ["2", "1", "t"] $ 0.8 * 0.5 * 0.6,
--   Tuple ["2", "1", "f"] $ 0.8 * 0.5 * 0.4,
--   Tuple ["2", "2", "t"] $ 0.8 * 0.5 * 0.7,
--   Tuple ["2", "2", "f"] $ 0.8 * 0.5 * 0.3
--   ]

-- dist3C = f [
--   Tuple ["1", "1", "t"] $ 0.2 * 0.4 * 0.5,
--   Tuple ["1", "1", "f"] $ 0.2 * 0.4 * 0.5,
--   Tuple ["1", "2", "t"] $ 0.2 * 0.6 * 0.5,
--   Tuple ["1", "2", "f"] $ 0.2 * 0.6 * 0.5,
--   Tuple ["2", "1", "t"] $ 0.8 * 0.4 * 0.5,
--   Tuple ["2", "1", "f"] $ 0.8 * 0.4 * 0.5,
--   Tuple ["2", "2", "t"] $ 0.8 * 0.6 * 0.5,
--   Tuple ["2", "2", "f"] $ 0.8 * 0.6 * 0.5
--   ]



-- input = """
-- ----
-- a b | c P
-- ----
-- 1 1 | t 0.5
--     | f 0.5

-- 1 2 | t 0.6
--     | f 0.4

-- 2 1 | t 0.6
--     | f 0.4

-- 2 2 | t 0.7
--     | f 0.3

-- ----
-- | a P
-- ----
-- | 1 0.2
-- | 2 0.8

-- ----
-- | b P
-- ----
-- | 1 0.4
-- | 2 0.6
-- """

-- inputBase = """
-- ----
-- a b | c P
-- ----
-- 1 1 | t 0.5
--     | f 0.5

-- 1 2 | t 0.5
--     | f 0.5

-- 2 1 | t 0.5
--     | f 0.5

-- 2 2 | t 0.5
--     | f 0.5

-- ----
-- | a P
-- ----
-- | 1 0.5
-- | 2 0.5

-- ----
-- | b P
-- ----
-- | 1 0.5
-- | 2 0.5
-- """

cite :: PMFPair Variable State Variable State -> Result
cite (PMFPair c (PMF _ as)) =
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
    -- Combing these lines runs into purescript bug
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
    Tuple vs ss <- pure <<< unzip $ (\(Space v s) -> Tuple v s) <$> spaces
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
       (Arbitrary (Space a b), Arbitrary (Space c d),
        Arbitrary (CondPMF a b c d), Ord d) =>
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
