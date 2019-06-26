module DotLang where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.DotLang (Edge(..))
import Data.DotLang as Dot
import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Node as Node
import Data.Foldable as Foldable
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Map as Map
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartialBecause)
import Utility (piecesOfN)

highlightPaths :: forall k. Show k => Color -> Set (NonEmptyList k) -> Dot.Graph -> Dot.Graph
highlightPaths color paths g =
  Foldable.foldr (modifyPath <<< Array.cons $ Edge.Color color) g paths

modifyPath ::
  forall k.
  Show k =>
  (Array Edge.Attr -> Array Edge.Attr) -> NonEmptyList k -> Dot.Graph -> Dot.Graph
modifyPath f path g =
  case g of
    Dot.Graph defs -> Dot.Graph defs
    Dot.DiGraph defs -> Dot.DiGraph (map (\d -> if inPath d then modifyEdge f d else d) defs)
  where
    inPath d = Foldable.any (\(Tuple from to) -> isEdge from to d) edges
    edges =
      unsafePartialBecause "piecesOfN guarantees lists of 2" $
      map (\(Cons x (Cons y Nil)) -> Tuple x y) <<< piecesOfN 2 <<< List.fromFoldable $ path

isEdge :: forall k. Show k => k -> k -> Dot.Definition -> Boolean
isEdge from to (Dot.EdgeDef (Edge _ fromS toS _)) =
  show from == fromS && show to == toS || show from == toS && show to == fromS
isEdge _ _ _ = false

modifyEdge :: (Array Edge.Attr -> Array Edge.Attr) -> Dot.Definition -> Dot.Definition
modifyEdge f (Dot.EdgeDef (Edge d from to as)) = Dot.EdgeDef <<< Edge d from to $ f as
modifyEdge f d = d

graphToGraph :: forall k v. Show k => (v -> String) -> Graph k v -> Dot.Graph
graphToGraph valueToLabel g =
  Dot.graphFromElements vertices edges
  where
    edges = map edgeToEdge <<< vertexToEdges =<< vertexList
    edgeToEdge (Tuple from to) = Dot.Edge Dot.Forward (show from) (show to) []
    vertices = vertexToVertex <$> vertexList
    vertexToVertex (Tuple k (Tuple v _)) =
      Dot.Node (show k)
        [ Node.Label $ valueToLabel v
        , Node.Shape Node.None
        ]
    vertexList = Map.toUnfoldable <<< Graph.toMap $ g
    vertexToEdges (Tuple k (Tuple _ es)) = Tuple k <$> Array.fromFoldable es
