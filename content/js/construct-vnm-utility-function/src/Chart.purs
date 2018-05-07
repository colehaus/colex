module Chart where

import Prelude

import Data.Argonaut.Core (JObject, fromArray, fromNumber, fromObject, fromString, jsonFalse) as Argo
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Argo
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.StrMap as StrMap
import Data.Tuple.Nested ((/\))
import Economics.Utility.VNM (smallest)

chartOpts :: Argo.JObject
chartOpts =
  StrMap.fromFoldable ["renderer" /\ Argo.fromString "svg", "actions" /\ Argo.jsonFalse]

newtype ChartInterval = MkChartInterval { good :: String, lower :: Number, upper :: Number }
derive instance genericChartInterval :: Generic ChartInterval
instance encodeChartInterval :: Argo.EncodeJson ChartInterval where
  encodeJson = encodeJson
instance showChartInterval :: Show ChartInterval where
  show = gShow

chartData :: List ChartInterval -> Argo.JObject
chartData intervals =
  StrMap.fromFoldable
    [ "datasets" /\ (obj ["main" /\ Argo.encodeJson intervals])
    , "data" /\ (obj ["name" /\ str "main"])
    ]
  where
    obj = Argo.fromObject <<< StrMap.fromFoldable
    str = Argo.fromString

chartSpec :: Number -> String -> Argo.JObject
chartSpec width good =
  StrMap.fromFoldable
    [ "$schema" /\ str "https://vega.github.io/schema/vega-lite/v2.json"
    , "width" /\ num (width - 100.0)
    , "height" /\ num (width * 0.4)
    , "layer" /\
      arr
        [ obj ["mark" /\ str "rule", "encoding" /\ Argo.fromObject ruleEncoding]
        , obj
            [ "mark" /\ str "tick"
            , "encoding" /\ Argo.fromObject (tickEncoding "upper")
            ]
        , obj
            [ "mark" /\ str "tick"
            , "encoding" /\ Argo.fromObject (tickEncoding "lower")
            ]
        ]
    ]
  where
    obj = Argo.fromObject <<< StrMap.fromFoldable
    arr = Argo.fromArray
    str = Argo.fromString
    num = Argo.fromNumber
    tickEncoding field = StrMap.fromFoldable [y, "x" /\ x field]
    y = "y" /\ obj ["field" /\ str "good", "type" /\ str "ordinal"]
    ruleEncoding = StrMap.fromFoldable [y, "x" /\ x "lower", "x2" /\ x "upper"]
    x field =
      obj
        [ "field" /\ str field
        , "type" /\ str "quantitative"
        , "scale" /\
          obj ["domain" /\ arr [num smallest, num 1.0], "type" /\ str "log"]
        , "axis" /\ obj ["title" /\ str ("value relative to " <> good)]
        ]
