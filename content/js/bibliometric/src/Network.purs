module Network where

import Data.Array as Array
import Data.Foldable as F
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as Set
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.NonEmpty.Indexed as Indexed
import Data.Traversable as T
import Data.Tuple (Tuple(..), fst, snd)
import Prelude
import Partial.Unsafe (unsafePartial)

import Math.Probability (just, reshape, uniform, (??))
import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Prob.Number (Prob(..))
import Math.Probability.Information (Entropy(..), entropy, nonCond)
import Network.Types (CondPMF, Network, condDeps, condMain, condMainSpace, condMap, condPmfMap, lookup, netList, netPmfAlter, spaceStates, spaceVar)

type Dist' = Dist Prob

inits :: forall a. Array a -> Array (Array a)
inits as = case Array.init as of
  Nothing -> [[]]
  Just as' -> Array.snoc (inits as') as

type GetProb a b c d = Network a b c d -> Set (Tuple a b) -> Prob

netScores :: forall a b c d. Ord a => Ord b =>
             GetProb a b c d -> Network a b c d -> Array (Tuple a Entropy)
netScores f n =
  unsafePartial $ Array.zip as $ Array.zipWith (\a b -> wrap $ a - b)
                     (fromJust $ Array.tail ns) ns where
    ns = unwrap <<< nonCond entropy <<< netDist f <<<
         flip uniformize n <$> inits as
    as = condMain <$> netList n
    unwrap (MkEntropy e) = e
    wrap e = MkEntropy e

uniformize :: forall a b c d. Ord a => Ord b => Array a -> Network a b c d -> Network a b c d
uniformize vs n =
  unsafePartial $ F.foldl (flip $ netPmfAlter $ condPmfMap $ reshape uniform) n vs

netDist :: forall a b c d. Ord a => Ord b =>
           GetProb a b c d -> Network a b c d -> Dist' (Set (Tuple a b))
netDist f n =
  unsafePartial $ fromJust <<< dist $ (identity &&& f n) <$> (asArray <<< Set.toUnfoldable) (netCombos n)
  where
    asArray :: forall x. Array x -> Array x
    asArray = identity
    dist = map Dist.make <<< nonEmptyMap <<< Map.fromFoldable
    nonEmptyMap = Indexed.nonEmpty (\mp -> (\mn -> Tuple (Tuple mn.key mn.value) (Map.delete mn.key mp)) <$> Map.findMin mp)


netCombos :: forall a b c d. Ord a => Ord b =>
             Network a b c d -> Set (Set (Tuple a b))
netCombos =
  Set.fromFoldable <<< (<$>) Set.fromFoldable <<< T.sequence <<<
  (<$>) ((\s -> Tuple (spaceVar s) <$> spaceStates s) <<< condMainSpace) <<<
  netList

comboProb :: forall a b. Ord a => Ord b =>
             Network a b (Array a) (Array b) -> Set (Tuple a b) -> Maybe Prob
comboProb n os =
  prob <<< F.product =<<
  T.traverse (\o -> unwrap <$>
                    fullLookup (fst o) (prune (Set.delete o os)) (snd o) n)
             (Set.toUnfoldable os :: Array (Tuple a b)) where
    prune os' c =
      snd <$> Array.filter (flip Set.member (Set.fromFoldable $ condDeps c) <<< fst)
      (Set.toUnfoldable os' :: Array (Tuple a b))
    prob p | 0.0 <= p && p <= 1.0 = Just $ MkProb p
           | otherwise = Nothing


fullLookup :: forall a b c d. Ord a => Ord b => Ord d =>
              a -> (CondPMF a b c d -> d) -> b -> Network a b c d -> Maybe Prob
fullLookup a f b n = do
  c <- a `lookup` n
  (??) (just b) <$> f c `Map.lookup` condMap c
