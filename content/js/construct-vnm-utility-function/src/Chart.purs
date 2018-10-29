module Chart where

import Prelude

import Data.Array as Array
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (fromArray, fromNumber, fromObject, fromString, jsonFalse, toArray, toObject) as Argo
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Argo
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Economics.Utility.VNM (smallest)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartialBecause)

chartOpts :: Object Json
chartOpts =
  Object.fromFoldable ["renderer" /\ Argo.fromString "svg", "actions" /\ Argo.jsonFalse]

newtype ChartInterval = MkChartInterval { good :: String, lower :: Number, upper :: Number }
derive instance genericChartInterval :: Generic ChartInterval _
instance encodeChartInterval :: Argo.EncodeJson ChartInterval where
  encodeJson = genericEncodeJson
instance showChartInterval :: Show ChartInterval where
  show = genericShow

extractRecord :: Object Json -> Object Json
extractRecord obj =
  unsafePartialBecause "Static format" $ fromJust $
    Argo.toObject <=< Array.head <=< Argo.toArray <=< Object.lookup "values" $ obj

chartData :: List ChartInterval -> Object Json
chartData intervals =
  Object.fromFoldable
    [ "datasets" /\ (obj ["main" /\ Argo.encodeJson (intervalToJson <$> intervals)])
    , "data" /\ (obj ["name" /\ str "main"])
    ]
  where
    intervalToJson x = unsafePartialBecause "Static format" $ extractRecord <<< fromJust <<< Argo.toObject <<< Argo.encodeJson $ x
    obj = Argo.fromObject <<< Object.fromFoldable
    str = Argo.fromString

chartSpec :: Number -> String -> Object Json
chartSpec width good =
  Object.fromFoldable
    [ "$schema" /\ str "https://vega.github.io/schema/vega-lite/v3.json"
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
    obj = Argo.fromObject <<< Object.fromFoldable
    arr = Argo.fromArray
    str = Argo.fromString
    num = Argo.fromNumber
    tickEncoding field = Object.fromFoldable [y, "x" /\ x field]
    y = "y" /\ obj ["field" /\ str "good", "type" /\ str "ordinal"]
    ruleEncoding = Object.fromFoldable [y, "x" /\ x "lower", "x2" /\ x "upper"]
    x field =
      obj
        [ "field" /\ str field
        , "type" /\ str "quantitative"
        , "scale" /\
          obj ["domain" /\ arr [num smallest, num 1.0], "type" /\ str "log"]
        , "axis" /\ obj ["title" /\ str ("value relative to " <> good)]
        ]
