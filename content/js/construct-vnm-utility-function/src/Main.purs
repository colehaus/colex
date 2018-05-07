module Main where

import Prelude hiding (bottom,top)

import Charts.Vega.Primitive as Vega
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (preventDefault, ready) as J
import Control.Monad.Eff.JQuery.Fancy
  ( clearOne
  , getProp
  , getText
  , getValue
  , on
  , selectOne
  , selectOnes
  , setText
  , width
  ) as J
import Control.Monad.Eff.JQuery.Fancy (JQuery, One)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import DOM (DOM)
import DOM.Event.Types (EventType)
import Data.Argonaut.Core
  ( JObject
  , fromArray
  , fromNumber
  , fromObject
  , fromString
  , jsonFalse
  , toObject
  ) as Argo
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Argo
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Either (Either(..), either, hush, note)
import Data.Filterable (filter)
import Data.Foreign (unsafeFromForeign)
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Number.Format (exponential, toStringWith)
import Data.Set as Set
import Data.StrMap as StrMap
import Data.String as String
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable)
import Economics.Utility.Ratio (Pair(..), Ratio(..))
import Economics.Utility.Ratio as Ratio
import Economics.Utility.VNM (nonEmptySet, pickNextLottery, refine, smallest)
import Economics.Utility.VNM.Function
  ( UtilityFn
  , best
  , byGood
  , goodsToInitialFn
  , keepBase
  )
import FRP (FRP)
import FRP.Event (Event)
import FRP.Event as FRP
import Math.Interval.Bound (Bound(..))
import Math.Interval.Internal (Interval(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)

chartOpts :: Argo.JObject
chartOpts =
  StrMap.fromFoldable ["renderer" /\ Argo.fromString "svg", "actions" /\ Argo.jsonFalse]

newtype GraphInterval = MkGraphInterval { good :: String, lower :: Number, upper :: Number }
derive instance genericGraphInterval :: Generic GraphInterval
instance encodeGraphInterval :: Argo.EncodeJson GraphInterval where
  encodeJson = encodeJson
instance showGraphInterval :: Show GraphInterval where
  show = gShow

chartData :: List GraphInterval -> Argo.JObject
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

main :: forall e. Eff (dom :: DOM, frp :: FRP, ref :: REF | e) Unit
main =
  unsafePartialBecause "Initial text known to be good" $
  J.ready do
    elements <- collectElements
    ordering <- response elements.decisionInputs
    utilityFn <- utilityFn elements.initialInputs
    let function =
          FRP.fold
            update
            ((Left <$> FRP.filterMap hush utilityFn.event) <|> (Right <$> ordering))
            utilityFn.initial
    void $ FRP.subscribe (pickNextLottery <$> function) (sinkLottery elements . decisionDisplays)
    void <<< FRP.subscribe function <<< sinkFunction elements.visualizationDisplays =<< newRef Nothing

update ::
     forall a.
     Ord a
  => Either (UtilityFn a Number) Ordering
  -> UtilityFn a Number
  -> UtilityFn a Number
update evt oldFn =
  maybePrune $ either id (flip (refine (pickNextLottery oldFn)) oldFn) evt
  where
    maybePrune f = maybe f (flip keepBase f) <<< best $ f

collectElements :: forall e. Eff (dom :: DOM | e) Elements
collectElements = do
  initialInputs <- collectInitialInputs
  decisionInputs <- collectDecisionInputs
  decisionDisplays <- collectDecisionDisplays
  visualizationDisplays <- collectVisualizationDisplays
  pure { initialInputs, decisionInputs, decisionDisplays, visualizationDisplays }

collectInitialInputs :: forall e. Eff (dom :: DOM | e) InitialInputs
collectInitialInputs =
  unsafePartialBecause "We require this element structure to function" do
    Just goods <- J.selectOne "#goods"
    pure { goods }

collectDecisionInputs :: forall e. Eff (dom :: DOM | e) DecisionInputs
collectDecisionInputs =
  unsafePartialBecause "We require this element structure to function" do
    Just first <- J.selectOne "#first-good"
    Just indifferent <- J.selectOne "#indifferent"
    Just second <- J.selectOne "#second-good"
    pure { first, indifferent, second }

response ::
     forall e.
     DecisionInputs
  -> Eff (dom :: DOM, frp :: FRP | e) (Event Ordering)
response els = do
  first <- jqueryEvent (wrap "click") (\_ -> pure GT) els.first
  indifferent <- jqueryEvent (wrap "click") (\_ -> pure EQ) els.indifferent
  second <- jqueryEvent (wrap "click") (\_ -> pure LT) els.second
  pure (first <|> indifferent <|> second)

type DecisionDisplays =
  { firstChance :: JQuery (One "span")
  , secondChance :: JQuery (One "span")
  , firstGood :: JQuery (One "span")
  , secondGood :: JQuery (One "span")
  }

type VisualizationDisplays =
  { functionVisualization :: JQuery (One "div")
  }

collectDecisionDisplays ::
  forall e. Eff (dom :: DOM | e) DecisionDisplays
collectDecisionDisplays =
  unsafePartialBecause "We require this element structure to function" do
    [firstChance, secondChance] <- J.selectOnes ".odds"
    [firstGood, secondGood] <- J.selectOnes ".good"
    pure { firstChance, secondChance, firstGood, secondGood }

collectVisualizationDisplays ::
  forall e. Eff (dom :: DOM | e) VisualizationDisplays
collectVisualizationDisplays =
  unsafePartialBecause "We require this element structure to function" do
    Just functionVisualization <- J.selectOne "#function-visualization"
    pure {functionVisualization}

sinkLottery ::
     forall e.
     DecisionDisplays
  -> Ratio String Number
  -> Eff (dom :: DOM | e) Unit
sinkLottery els lottery = do
  J.setText (Ratio.base lottery) els.firstGood
  J.setText (Ratio.quote lottery) els.secondGood
  J.setText (show' base') els.firstChance
  J.setText (show' quote') els.secondChance
  where
    show' = toStringWith (exponential 2)
    val = (unwrap lottery).relativeValue
    base' | val < 1.0 = 1.0 / val
          | otherwise = 1.0
    quote' | val < 1.0 = 1.0
           | otherwise = val

sinkFunction ::
     forall e.
     VisualizationDisplays
  -> Ref (Maybe Vega.View)
  -> UtilityFn String Number
  -> Eff (dom :: DOM, ref :: REF | e) Unit
sinkFunction els ref fn = do
  case (\best' -> Tuple best' <$> Map.lookup best' (byGood fn)) =<< best fn of
    Nothing -> clear
    Just (Tuple best' ratios) -> do
      let intervals :: forall f. Unfoldable f => Functor f => f GraphInterval
          intervals = toGraphInterval best' <$> Set.toUnfoldable ratios
      maybe (createViewWithData best' intervals) (flip changeData intervals) =<< readRef ref
  where
    createViewWithData best' intervals = do
          width <- J.width els.functionVisualization
          id <- unsafeFromForeign <$> J.getProp "id" els.functionVisualization
          Vega.embed
            ("#" <> id)
            (Argo.fromObject $ StrMap.union (chartSpec width best') (chartData intervals))
            (Argo.fromObject chartOpts)
            (writeRef ref <<< Just)

    changeData view intervals =
      Vega.changeData
            "main"
            (Both {remove: Vega.ByPredicate $ const true} {insert: encodeAsObject <$> intervals })
            view
    clear = do
      J.clearOne els.functionVisualization
      writeRef ref Nothing
    encodeAsObject = unsafeFromJustBecause "Static object" <<< Argo.toObject <<< Argo.encodeJson
    toGraphInterval best'
      (MkRatio
        { pair: MkPair {base, quote}
        , relativeValue:
          NonEmpty
            { lower: Finite {bound: lowerBound}
            , upper: Finite {bound: upperBound}
            }
        }
      ) =
       MkGraphInterval $
       if best' == base
         then {good: quote, lower: 1.0 / upperBound, upper: 1.0 / lowerBound}
         -- We bump `lower` by smallest because log scale can't handle 0
         else {good: base, lower: lowerBound + smallest, upper: upperBound}
    toGraphInterval _ _ = unsafeCrashWith "Presence of 'best' implies these properties"

jqueryEvent ::
     forall a e tag.
     EventType
  -> (Unit -> Eff (frp :: FRP, dom :: DOM | e) a)
  -> JQuery (One tag)
  -> Eff (dom :: DOM , frp :: FRP | e) (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\evt _ -> push =<< f unit <* J.preventDefault evt) el
  pure event

textAreaEvent ::
     forall e.
     JQuery (One "textarea")
  -> Eff (dom :: DOM , frp :: FRP | e) (Event String)
textAreaEvent el =
  jqueryEvent (wrap "input") (\_ -> unsafeFromForeign <$> J.getValue el) el

type InitialInputs =
  { goods :: JQuery (One "textarea")
  }

type DecisionInputs =
  { first :: JQuery (One "input")
  , indifferent :: JQuery (One "input")
  , second :: JQuery (One "input")
  }

type Elements =
  { initialInputs :: InitialInputs
  , decisionInputs :: DecisionInputs
  , decisionDisplays :: DecisionDisplays
  , visualizationDisplays :: VisualizationDisplays
  }

utilityFn ::
     forall e.
     InitialInputs
  -> Eff
      (dom :: DOM , frp :: FRP | e)
      { initial :: UtilityFn String Number
      , event :: Event (Either Unit (UtilityFn String Number))
      }
utilityFn els =
  unsafePartialBecause "Starting input is known to be safe" do
    Right initial <-
      goodsToInitialFn' <<<
      filter (_ /= mempty) <<<
      String.split (wrap "\n") <$> J.getText els.goods
    event <-
      map
        (goodsToInitialFn' <<< filter (_ /= mempty) <<< String.split (wrap "\n")) <$>
      textAreaEvent els.goods
    pure {initial, event}
  where
    goodsToInitialFn' =
      goodsToInitialFn <=< note unit <<< nonEmptySet <<< Set.fromFoldable

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str
