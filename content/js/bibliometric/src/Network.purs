module Network where

import Data.Array as A
import Data.Foldable as F
import Data.Profunctor.Strong ((&&&))
import Data.Set as S
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable as T
import Data.Tuple (Tuple(..), fst, snd)
import Prelude
import Partial.Unsafe (unsafePartial)

import Math.Probability (Dist, Prob, dist, from, just, prob, reshape, runProb, to, uniform, (??))
import Math.Probability.Information (Entropy, entropy, entropyNum, nonCond)
import Network.Types (CondPMF, Network, condDeps, condMain, condMainSpace, condMap, condPmfMap, lookup, netList, netPmfAlter, spaceStates, spaceVar)

inits :: forall a. Array a -> Array (Array a)
inits as = case A.init as of
  Nothing -> [[]]
  Just as' -> A.snoc (inits as') as

type GetProb a b c d = Network a b c d -> S.Set (Tuple a b) -> Prob

netScores :: forall a b c d. Ord a => Ord b =>
             GetProb a b c d -> Network a b c d -> Array (Tuple a Entropy)
netScores f n =
  unsafePartial $ A.zip as $ A.zipWith (\a b -> from entropyNum $ a - b)
                     (fromJust $ A.tail ns) ns where
    ns = to entropyNum <<< nonCond entropy <<< netDist f <<<
         flip uniformize n <$> inits as
    as = condMain <$> netList n

uniformize :: forall a b c d. Ord a => Array a -> Network a b c d -> Network a b c d
uniformize vs n =
  unsafePartial $ F.foldl (flip (netPmfAlter (condPmfMap (fromJust <<< reshape uniform)))) n vs

netDist :: forall a b c d. Ord a => Ord b =>
           GetProb a b c d -> Network a b c d -> Dist (S.Set (Tuple a b))
netDist f n =
  unsafePartial $ fromJust <<< dist $ (id &&& f n) <$> S.toUnfoldable (netCombos n)

netCombos :: forall a b c d. Ord a => Ord b =>
             Network a b c d -> S.Set (S.Set (Tuple a b))
netCombos =
  S.fromFoldable <<< (<$>) S.fromFoldable <<< T.sequence <<<
  (<$>) ((\s -> Tuple (spaceVar s) <$> spaceStates s) <<< condMainSpace) <<<
  netList

comboProb :: forall a b. Ord a => Ord b =>
             Network a b (Array a) (Array b) -> S.Set (Tuple a b) -> Maybe Prob
comboProb n os =
  prob <<< F.product =<<
  T.traverse (\o -> runProb <$>
                    fullLookup (fst o) (prune (S.delete o os)) (snd o) n)
             (S.toUnfoldable os :: Array (Tuple a b)) where
    prune os' c =
      snd <$> A.filter (flip S.member (S.fromFoldable $ condDeps c) <<< fst)
      (S.toUnfoldable os' :: Array (Tuple a b))

fullLookup :: forall a b c d. Ord a => Eq b => Ord d =>
              a -> (CondPMF a b c d -> d) -> b -> Network a b c d -> Maybe Prob
fullLookup a f b n = do
  c <- a `lookup` n
  (??) (just b) <$> f c `M.lookup` condMap c
