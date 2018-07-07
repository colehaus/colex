module Chart where

import Prelude

import Data.Argonaut.Core (JObject, fromArray, fromNumber, fromObject, fromString, jsonFalse) as Argo
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Argo
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Generic (class Generic)
import Data.List (List)
import Data.StrMap as StrMap
import Data.Tuple.Nested ((/\))

opts :: Argo.JObject
opts =
  StrMap.fromFoldable ["renderer" /\ Argo.fromString "svg", "actions" /\ Argo.jsonFalse]

newtype Trial
  = MkTrial
  { stochastic :: Number
  , deterministic :: Number
  , stochasticCombined :: Number
  , deterministicCombined :: Number
  , combined :: Number
  }
derive instance genericTrial :: Generic Trial
instance encodeTrial :: Argo.EncodeJson Trial where
  encodeJson = encodeJson

mkData :: List Trial -> Argo.JObject
mkData trials =
  StrMap.fromFoldable
    [ "datasets" /\ (obj ["main" /\ Argo.encodeJson trials])
    , "data" /\ (obj ["name" /\ str "main"])
    ]
  where
    obj = Argo.fromObject <<< StrMap.fromFoldable
    str = Argo.fromString

mkSpec :: Number -> Argo.JObject
mkSpec width =
  StrMap.fromFoldable
    [ "$schema" /\ str "https://vega.github.io/schema/vega-lite/v2.json"
    , "width" /\ num (width - 100.0)
    , "height" /\ num (width * 0.4)
    , "mark" /\ str "point"
    , "encoding" /\ Argo.fromObject encoding
    ]
  where
    encoding = StrMap.fromFoldable ["y" /\ deterministic, "x" /\ deterministicCombined]
    deterministicCombined =
      obj
        [ "field" /\ str "deterministicCombined"
        , "type" /\ str "quantitative"
        , "axis" /\ obj ["title" /\ str ("Deterministic factors of best outcome")]
        ]
    deterministic =
      obj
        [ "field" /\ str "deterministic"
        , "type" /\ str "quantitative"
        , "axis" /\ obj ["title" /\ str ("Best deterministic factors")]
        ]
    size = obj [ "aggregate" /\ str "count", "type" /\ str "quantitative" ]
    obj = Argo.fromObject <<< StrMap.fromFoldable
    arr = Argo.fromArray
    str = Argo.fromString
    num = Argo.fromNumber

