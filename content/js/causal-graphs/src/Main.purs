module Main where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.DotLang (Attr(..), Edge(..), EdgeType(..), Graph, Node(..), ShapeType(..), graphFromElements) as Dot
import Data.Either (Either, either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Map as Map
import Data.Newtype (class Newtype, un)
import Data.Newtype as Newtype
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Yaml (parseFromYaml)
import Effect (Effect)
import FRP.Event as FRP
import FRP.JQuery (textAreaChangeEvent)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Graphics.Graphviz (Engine(..), renderToSvg)
import Html (Elements)
import Html as Html
import JQuery (append, create, ready) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (clearOne, getValue, setText) as J

foreign import jQuery :: Foreign

main :: Effect Unit
main =
  J.ready do
    els <- Html.collectElements
    wireUp analyze els

newtype Svg = MkSvg String
derive instance newtypeSvg :: Newtype Svg _

wireUp ::
  (String -> Either String Svg) ->
  Elements ->
  Effect (Effect Unit)
wireUp analyze' els = do
  initialText <- unsafeFromForeign <$> J.getValue els.graphInput
  either (\e -> J.setText e els.graphOutput) (render els.graphOutput) <<< analyze' $
    initialText
  textAreaChangeEvent els.graphInput >>=
    flip FRP.subscribe
      (either (\e -> J.setText e els.graphOutput) (render els.graphOutput) <<< analyze')

render :: JQuery (One "div") -> Svg -> Effect Unit
render output svg = do
  J.clearOne output
  el <- J.create $ un MkSvg svg
  J.append el (Newtype.unwrap output)

analyze :: String -> Either String Svg
analyze = map (graphToSvg identity <<< fromObject) <<< decodeJson <=< parseFromYaml
  where
    fromObject :: Object (Array String) -> Graph String String
    fromObject =
      Graph.fromMap <<< mapWithIndex (\k ks -> Tuple k (Set.fromFoldable ks)) <<<
      Map.fromFoldable <<< asArray <<< Object.toUnfoldable
    asArray :: forall a. Array a -> Array a
    asArray = identity

graphToSvg :: forall k v. Show k => (v -> String) -> Graph k v -> Svg
graphToSvg valueToLabel = MkSvg <<< renderToSvg Dot <<< graphToGraph valueToLabel

graphToGraph :: forall k v. Show k => (v -> String) -> Graph k v -> Dot.Graph
graphToGraph valueToLabel g =
  Dot.graphFromElements vertices edges
  where
    edges = map edgeToEdge <<< vertexToEdges =<< vertexList
    edgeToEdge (Tuple from to) = Dot.Edge Dot.Forward (show from) (show to)
    vertices = vertexToVertex <$> vertexList
    vertexToVertex (Tuple k (Tuple v _)) =
      Dot.Node (show k)
        [ Dot.Label $ valueToLabel v
        , Dot.Shape Dot.None
        ]
    vertexList = Map.toUnfoldable <<< Graph.toMap $ g
    vertexToEdges (Tuple k (Tuple _ es)) = Tuple k <$> Array.fromFoldable es

graph :: Graph Int Int
graph =
  Graph.fromMap (
    Map.fromFoldable
      [ n 1 [ 2 ]
      , n 2 [ 3, 4 ]
      , n 3 [ 5, 6 ]
      , n 4 [ 8 ]
      , n 5 [ 7 ]
      , n 6 [ ]
      , n 7 [ ]
      , n 8 [ 5 ]
      ])
  where
    n k v = Tuple k (Tuple k (Set.fromFoldable v ))
