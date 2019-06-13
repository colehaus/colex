module Utility where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Graph.Causal as Causal
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd, uncurry)

newtype UnorderedTuple a = MkUnorderedTuple (Tuple a a)
instance eqUnorderedTuple :: Ord a => Eq (UnorderedTuple a) where
  eq = eq `on` (sortTuple <<< un MkUnorderedTuple)
instance ordUnorderedTuple :: (Ord a) => Ord (UnorderedTuple a) where
  compare = compare `on` (sortTuple <<< un MkUnorderedTuple)
derive instance newtypeUnorderedTuple :: Newtype (UnorderedTuple a) _

sortTuple :: forall a. Ord a => Tuple a a -> Tuple a a
sortTuple (Tuple a b)
  | a <= b = Tuple a b
  | otherwise = Tuple b a

distinctUnorderedPairs :: forall a. Ord a => Set a -> Set (UnorderedTuple a)
distinctUnorderedPairs as =
  Set.filter (uncurry (/=) <<< un MkUnorderedTuple) <<< Set.fromFoldable $
  (\l r -> MkUnorderedTuple $ Tuple l r) <$> Array.fromFoldable as <*> Array.fromFoldable as

distinctPairs :: forall a. Ord a => Set a -> Set (Tuple a a)
distinctPairs as =
  Set.filter (uncurry (/=)) <<< Set.fromFoldable $
  Tuple <$> Array.fromFoldable as <*> Array.fromFoldable as

piecesOfN :: forall a. Int -> List a -> List (List a)
piecesOfN n l =
  case Tuple (Foldable.length l < n) (List.tail l) of
    Tuple true _ -> Nil
    Tuple _ Nothing -> Nil
    Tuple _ (Just tail') -> List.take n l `Cons` piecesOfN n tail'

powerSet :: forall a. Ord a => Set a -> Set (Set a)
powerSet =
  Set.fromFoldable <<< map Set.fromFoldable <<<
  List.filterM (const [true, false]) <<<
  List.fromFoldable

-- If the user has one vertex point to another vertex that they haven't explicitly defined,
-- add that implicit vertex in with no outgoing edges
closeGraph :: forall k. Ord k => Map k (Tuple k (Set k)) -> Map k (Tuple k (Set k))
closeGraph m = m `Map.union` Map.fromFoldable (Set.map mkOrphanVertex orphanEdgeVertices)
  where
    mkOrphanVertex k = Tuple k (Tuple k Set.empty)
    orphanEdgeVertices = edgeVertices `Set.difference` Map.keys m
    edgeVertices = Set.fromFoldable $ Set.toUnfoldable <<< snd =<< Map.values m

dSeparations :: forall k v. Ord k => Graph k v -> Set (UnorderedTuple k)
dSeparations g =
  Set.filter (\(MkUnorderedTuple (Tuple k1 k2)) -> Causal.isDSeparated k1 k2 Set.empty g) <<<
  distinctUnorderedPairs <<<
  Map.keys <<< Graph.toMap $ g
