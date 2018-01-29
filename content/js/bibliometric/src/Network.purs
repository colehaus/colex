module Network where

import Control.Arrow
import Control.Bind
import Control.Monad.Error.Class
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Either
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Traversable as T
import Data.Tuple

import Math.Probability
import Math.Probability.Information
import Network.Types
import Network.Types.Internal (throwPmf)

inits as = case A.init as of
  Nothing -> [[]]
  Just as' -> A.snoc (inits as') as

type GetProb a b c d = Network a b c d -> S.Set (Tuple a b) -> Prob

netScores :: forall a b c d. (Ord a, Ord b) => 
             GetProb a b c d -> Network a b c d -> [Tuple a Entropy]
netScores f n =
  zip as $ A.zipWith (\a b -> from entropyNum $ a - b)
                     (fromJust $ A.tail ns) ns where
    ns = to entropyNum <<< nonCond entropy <<< netDist f <<<
         flip uniformize n <$> inits as
    as = condMain <$> netList n

uniformize :: forall a b c d. (Ord a) => [a] -> Network a b c d -> Network a b c d
uniformize vs n = 
  F.foldl (flip (netPmfAlter (condPmfMap (fromJust <<< reshape uniform)))) n vs

netDist :: forall a b c d. (Ord a, Ord b) =>
           GetProb a b c d -> Network a b c d -> Dist (S.Set (Tuple a b))
netDist f n =
  fromJust <<< dist $ (id &&& f n) <$> S.toList (netCombos n)

netCombos :: forall a b c d. (Ord a, Ord b) =>
             Network a b c d -> S.Set (S.Set (Tuple a b))
netCombos =
  S.fromList <<< (<$>) S.fromList <<< T.sequence <<<
  (<$>) ((\s -> Tuple (spaceVar s) <$> spaceStates s) <<< condMainSpace) <<<
  netList

comboProb :: forall a b c d. (Ord a, Ord b) =>
             Network a b [a] [b] -> S.Set (Tuple a b) -> Maybe Prob
comboProb n os =
  prob <<< F.product =<<
  T.traverse (\o -> runProb <$>
                    fullLookup (fst o) (prune (S.delete o os)) (snd o) n)
             (S.toList os) where
    prune os c =
      snd <$> A.filter (flip S.member (S.fromList $ condDeps c) <<< fst)
      (S.toList os)

fullLookup :: forall a b c d. (Ord a, Eq b, Ord d) =>
              a -> (CondPMF a b c d -> d) -> b -> Network a b c d -> Maybe Prob
fullLookup a f b n = do
  c <- a `lookup` n
  (??) (just b) <$> f c `M.lookup` condMap c
