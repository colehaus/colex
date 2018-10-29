module Chart where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (fromArray, fromNumber, fromObject, fromString, jsonFalse, toArray, toObject) as Argo
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Argo
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartialBecause)

opts :: Object Json
opts =
  Object.fromFoldable ["renderer" /\ Argo.fromString "svg", "actions" /\ Argo.jsonFalse]

newtype Trial
  = MkTrial
  { stochastic :: Number
  , deterministic :: Number
  , stochasticCombined :: Number
  , deterministicCombined :: Number
  , combined :: Number
  }
derive instance genericTrial :: Generic Trial _
instance encodeTrial :: Argo.EncodeJson Trial where
  encodeJson = genericEncodeJson

extractRecord :: Object Json -> Object Json
extractRecord obj =
  unsafePartialBecause "Static format" $ fromJust $
    Argo.toObject <=< Array.head <=< Argo.toArray <=< Object.lookup "values" $ obj

mkData :: List Trial -> Object Json
mkData trials =
  Object.fromFoldable
    [ "datasets" /\ (obj ["main" /\ Argo.encodeJson (trialToJson <$> trials)])
    , "data" /\ (obj ["name" /\ str "main"])
    ]
  where
    trialToJson x = unsafePartialBecause "Static format" $ extractRecord <<< fromJust <<< Argo.toObject <<< Argo.encodeJson $ x
    obj = Argo.fromObject <<< Object.fromFoldable
    str = Argo.fromString

mkSpec :: Number -> Object Json
mkSpec width =
  Object.fromFoldable
    [ "$schema" /\ str "https://vega.github.io/schema/vega-lite/v2.json"
    , "width" /\ num (width - 100.0)
    , "height" /\ num (width * 0.4)
    , "mark" /\ str "point"
    , "encoding" /\ Argo.fromObject encoding
    ]
  where
    encoding = Object.fromFoldable ["y" /\ deterministic, "x" /\ deterministicCombined]
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
    obj = Argo.fromObject <<< Object.fromFoldable
    arr = Argo.fromArray
    str = Argo.fromString
    num = Argo.fromNumber

