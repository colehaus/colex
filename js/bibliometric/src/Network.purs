module Network where

import Control.Arrow
import Control.Bind
import Control.Monad.Error.Class
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Traversable as T
import Data.Tuple

import Probability
import Probability.Information
import Network.Types

netScores :: Network Variable State [Variable] [State] ->
             S.Set (Tuple Variable Entropy)
netScores n =
  S.fromList <<< (<$>) (\ts ->
    Tuple (fst $ AU.head ts) <<<
    from entropyNum <<< F.sum $ (to entropyNum <<< snd) <$> ts) <<<
  A.groupBy ((==) `on` fst) <<< A.sortBy (compare `on` fst) <<<
  F.mconcat <<< fromJust <<< T.traverse (`scores` n) $ netVars n

scores :: Variable -> Network Variable State [Variable] [State] ->
          Maybe [Tuple Variable Entropy]
scores v n = do
  ds <- dependencies v n
  tds <- transitiveDeps v n
  let ve = to entropyNum $ gain [v] [] n
  let ts =
        (\d -> Tuple d <<< from entropyNum $
               (to entropyNum $ gain [d,v] [d] n) - ve) <$> ds
  let t =
        Tuple v <<< from entropyNum $
        to entropyNum (gain (v : tds) tds n) -
        (F.sum $ (to entropyNum <<< snd) <$> ts)
  pure $ t : ts

gain :: [Variable] -> [Variable] -> Network Variable State [Variable] [State] ->
        Entropy
gain l r n =
  on (divergence (pure unit))
     (const <<< netDist <<< flip uniformize n <<< complement)
     l r where
    complement = S.toList <<< S.difference (S.fromList $ netVars n) <<< S.fromList
    uniformize =
      flip (F.foldl (\n v -> fromJust $ netPmfAlter (condPmfMap (reshape uniform)) v n))

dependencies :: forall a b c d. (Ord a) => a -> Network a b c d -> Maybe c
dependencies a n = condDeps <$> a `lookup` n

transitiveDeps :: forall a b c d. (Ord a) => a -> Network a b [a] d -> Maybe [a]
transitiveDeps a n = do
  c <- dependencies a n
  c' <- F.mconcat <$> T.traverse (`transitiveDeps` n) c
  pure <<< setNub $ c <> c'

netDist :: Network Variable State [Variable] [State] -> Dist (S.Set Outcome)
netDist n =
  fromJust <<< dist $ (id &&& (fromJust <<< comboProb n)) <$>
  S.toList (netCombos n)

netCombos :: forall c d. Network Variable State c d -> S.Set (S.Set Outcome)
netCombos =
  S.fromList <<< (<$>) S.fromList <<< T.sequence <<<
  (<$>) ((\s -> Outcome (spaceVar s) <$> spaceStates s) <<< condMainSpace) <<<
  netList

comboProb :: Network Variable State [Variable] [State] ->
             S.Set Outcome -> Maybe Prob
comboProb n os =
  prob <<< F.product =<<
  T.traverse (\o -> runProb <$> fullLookup o (S.delete o os) n) (S.toList os)

fullLookup :: Outcome -> S.Set Outcome ->
              Network Variable State [Variable] [State] -> Maybe Prob
fullLookup (Outcome a s) os n = do
  c <- a `lookup` n
  (??) (just s) <$> prune (S.fromList $ condDeps c) os `M.lookup` condMap c where
    prune vs =
      (<$>) outcomeState <<< A.filter (flip S.member vs <<< outcomeVar) <<< S.toList
