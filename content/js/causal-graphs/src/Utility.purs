module Utility where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as LL
import Data.List.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Yaml (parseFromYaml)
import Foreign.Object (Object)
import Foreign.Object as Object

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

-- Like the above, but lazier
powerSet' :: forall a. Ord a => Set a -> Lazy.List (Set a)
powerSet' =
  map Set.fromFoldable <<<
  LL.filterM (const $ LL.fromFoldable [true, false]) <<< LL.fromFoldable

-- If the user has one vertex point to another vertex that they haven't explicitly defined,
-- add that implicit vertex in with no outgoing edges
closeGraph :: forall k. Ord k => Map k (Tuple k (Set k)) -> Map k (Tuple k (Set k))
closeGraph m = m `Map.union` Map.fromFoldable (Set.map mkOrphanVertex orphanEdgeVertices)
  where
    mkOrphanVertex k = Tuple k (Tuple k Set.empty)
    orphanEdgeVertices = edgeVertices `Set.difference` Map.keys m
    edgeVertices = Set.fromFoldable $ Set.toUnfoldable <<< snd =<< Map.values m

stringToGraph :: String -> Either String (Graph String String)
stringToGraph = map fromObject <<< decodeJson <=< parseFromYaml
  where
    fromObject :: Object (Array String) -> Graph String String
    fromObject =
      Graph.fromMap <<< closeGraph <<<
      mapWithIndex (\k ks -> Tuple k (Set.fromFoldable ks)) <<<
      Map.fromFoldable <<< asArray <<< Object.toUnfoldable
    asArray :: forall a. Array a -> Array a
    asArray = identity

vertexInGraph :: forall k v. Ord k => k -> Graph k v -> Boolean
vertexInGraph k = Set.member k <<< Map.keys <<< Graph.toMap
