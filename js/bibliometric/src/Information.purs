module Information where

import Control.Bind
import qualified Control.Monad.State as ST
import qualified Control.Monad.State.Class as ST
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid.All
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Tuple
import Math

import Information.Types

logBase :: Number -> Number -> Number
logBase b n = log n / log b

pmfEntropy :: PMF -> Entropy
pmfEntropy = entropy <<< M.values <<< pmfBody

entropy :: [Prob] -> Entropy  
entropy =
  Entropy <<< F.sum <<< (<$>) (\p -> let n = runProb p in -n * logBase 2 n)

networkEntropy :: Network -> Entropy
networkEntropy n = 
  entropy <<< fromJust <<< T.traverse (comboProb n) <<< S.toList <<< combos <<<
  S.fromList <<< (<$>) (uncurry condMainSpace) <<< M.toList $ runNet n 

comboProb :: Network -> Deps -> Maybe Prob
comboProb n ds =
  T.traverse (\o -> runProb <$> lookup n o (fromJust <<< deps $ S.delete o os))
  (S.toList os) >>=
  prob <<< F.product where
    os = runDeps ds

lookup :: Network -> Outcome -> Deps -> Maybe Prob
lookup n (Outcome v s) ds =
  case v `M.lookup` runNet n of
    Nothing -> Nothing
    Just c -> (M.lookup s <=< flip M.lookup c) $ prune c ds where
        prune :: CondPMFBody -> Deps -> Deps
        prune c =
          fromJust <<< depsAlter (A.filter $ flip S.member vs <<< outcomeVar) where
          vs = depVars c

picks :: forall a. (Eq a) => [a] -> [Tuple a [a]]
picks as = (\a -> Tuple a $ A.delete a as) <$> as 

permutations :: forall a. (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
  Tuple x xs' <- picks xs
  (:) x <$> permutations xs'

uniformPmf :: PMFBody -> [Prob]
uniformPmf b = const p <$> M.keys b where
  p = fromJust <<< prob $ 1 / M.size b

aggregateInfo :: Network -> S.Set Variable -> S.Set Variable -> Maybe Entropy
aggregateInfo n vs gs = do
  n' <- uniformize us n
  e' <- networkEntropy <$> uniformize vs n'
  let e = networkEntropy n'
  return <<< Entropy $  (runEntropy e') - (runEntropy e) where
    us = (S.fromList <<< M.keys $ runNet n) `S.difference` vs `S.difference` gs

info :: Network -> S.Set (Tuple Variable Entropy)
info n =
  S.fromList <<< avg <<< (<$>) (\(Tuple v vs) -> Tuple v <<< fromJust <<<
                                                 aggregateInfo n (S.singleton v) $
                                                 S.fromList vs) <<<
  A.concatMap chain $ permutations vs where
    vs = M.keys $ runNet n
    avg = (<$>) (\xs -> Tuple (fst $ AU.head xs) <<< Entropy <<< average $
                        runEntropy <<< snd <$> xs) <<<
          A.groupBy ((==) `on` fst) <<< A.sortBy (compare `on` fst)

average :: [Number] -> Number
average ns = F.sum ns / A.length ns

uniformize :: S.Set Variable -> Network -> Maybe Network
uniformize vs n | let vs' = S.fromList <<< M.keys $ runNet n in
  runAll <<< F.mconcat $ (All <<< flip S.member vs') <$> S.toList vs =
    Just <<< F.foldl (\acc a -> fromJust $ netPmfAlter uniformPmf a acc) n $
    S.toList vs
uniformize _ _ | otherwise = Nothing

chain :: forall a. [a] -> [Tuple a [a]]
chain = unfoldr f where
  f [] = Nothing
  f (b : bs) = Just (Tuple (Tuple b bs) bs)

unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> [a]
unfoldr f b =
  case f b of
    Just (Tuple a b') -> a : unfoldr f b'
    Nothing -> []

-- uniformNet :: Variable -> Network -> Network
-- uniformNet v n = fromJust <<< network $ x where
--   x :: M.Map Variable
--   x = M.alter ((<$>) f) v m 
--   m :: M.Map Variable CondPMFBody
--   m = runNet n
--   f :: CondPMFBody -> CondPMFBody
--   f = (<$>) uniformPMF
  
-- uniformCond :: CondPMFBody -> CondPMFBody
-- uniformCond c = fromJust <<< condPmf (condVar c) $ uniformPMF <$> condBody c

