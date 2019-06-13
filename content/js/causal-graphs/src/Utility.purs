module Utility where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd, uncurry)

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
