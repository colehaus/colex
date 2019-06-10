module Main where

import Prelude

import Color.Scheme.MaterialDesign (red)
import Data.DotLang (Attr(..), Definition(..), FillStyle(..), Graph(..), ShapeType(..), node, (==>))
import Data.Newtype as Newtype
import Effect (Effect)
import Foreign (Foreign)
import Graphics.Graphviz (Engine(..), renderToSvg)
import Html as Html
import JQuery (append, create, ready) as J

foreign import jQuery :: Foreign

main :: Effect Unit
main =
  J.ready do
    els <- Html.collectElements
    svgEl <- J.create svg
    J.append svgEl (Newtype.unwrap els.graphElement)

svg :: String
svg = renderToSvg Dot g

g :: Graph
g =
  DiGraph [
    node "a" [ Shape Diamond, Style Filled,  FillColor red ],
    node "b" [],
    "a" ==> "b",
    "a" ==> "d",
    Subgraph [
      node "d" []
    ]
  ]
