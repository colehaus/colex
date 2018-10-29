module Main where

import Prelude hiding (bottom,top)

import Chart (ChartInterval(..), chartData, chartOpts, chartSpec, extractRecord)
import Charts.Vega.Primitive as Vega
import Control.Alt ((<|>))
import Data.Argonaut.Core (fromObject, toObject) as Argo
import Data.Argonaut.Encode (encodeJson) as Argo
import Data.Array as Array
import Data.Either (Either(..), either, hush, note)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Number.Format (exponential, toStringWith)
import Data.Set as Set
import Data.String as String
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Economics.Utility.Ratio (Pair(..), Ratio(..))
import Economics.Utility.Ratio as Ratio
import Economics.Utility.VNM (isComplete, pickNextLottery, refine, smallest)
import Economics.Utility.VNM as VNM
import Economics.Utility.VNM.Function (UtilityFn, best, byGood, goodsToInitialFn, prune)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (unsafeFromForeign)
import Foreign.Object as Object
import FRP.Event (Event)
import FRP.Event as FRP
import FRP.JQuery (jqueryEvent, textAreaEvent)
import Helpers (nonEmptySet, unsafeFromJustBecause)
import Html (DecisionInputs, InitialInputs, VisualizationDisplays, DecisionDisplays, collectElements)
import JQuery (ready) as J
import JQuery.Fancy (clearOne, displayOne, getProp, getText, hide, hideOne, setText, width) as J
import Math.Interval (unmake)
import Math.Interval.Bound (Finite(MkFinite), finite)
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)

main :: Effect Unit
main =
  unsafePartialBecause "Initial text known to be good" $
  J.ready do
    elements <- collectElements
    ordering <- response elements.decisionInputs
    utilityFn' <- utilityFn elements.initialInputs
    let function =
          FRP.fold
            (\evt acc -> update evt =<< acc)
            ((Left <$> FRP.filterMap hush utilityFn' . event) <|>
             (Right <$> ordering))
            (Right utilityFn' . initial)
    void $ FRP.subscribe ordering sinkResponses
    void $
      FRP.subscribe
        (map pickNextLottery <$> function)
        (either
           (\_ -> sinkDone elements . decisionDisplays)
           (sinkLottery elements . decisionDisplays))
    (\r ->
       void <<< FRP.subscribe function $
       either
         (sinkFunction elements . visualizationDisplays r)
         (sinkFunction elements . visualizationDisplays r)) =<<
      Ref.new Nothing

update ::
     forall a.
     Ord a
  => Either (UtilityFn a Number) Ordering
  -> UtilityFn a Number
  -> Either (UtilityFn a Number) (UtilityFn a Number)
update evt oldFn =
  complete <<< maybePrune $ either identity (flip (refine (pickNextLottery oldFn)) oldFn) evt
  where
    maybePrune f = maybe f (flip prune f) <<< best $ f
    complete f =
      if isComplete f
      then Left f
      else Right f

response ::
     DecisionInputs
  -> Effect (Event Ordering)
response els = do
  first <- jqueryEvent (wrap "click") (\_ -> pure GT) els.first
  indifferent <- jqueryEvent (wrap "click") (\_ -> pure EQ) els.indifferent
  second <- jqueryEvent (wrap "click") (\_ -> pure LT) els.second
  pure $ first <|> indifferent <|> second

utilityFn ::
     InitialInputs
  -> Effect
      { initial :: UtilityFn String Number
      , event :: Event (Either Unit (UtilityFn String Number))
      }
utilityFn els =
  unsafePartialBecause "Starting input is known to be safe" do
    Right initial <-
      goodsToInitialFn' <<<
      Array.filter (_ /= mempty) <<<
      String.split (wrap "\n") <$> J.getText els.goods
    event <-
      map
        (goodsToInitialFn' <<< Array.filter (_ /= mempty) <<< String.split (wrap "\n")) <$>
      textAreaEvent els.goods
    pure {initial, event}
  where
    goodsToInitialFn' =
      goodsToInitialFn <=< note unit <<< nonEmptySet <<< Set.fromFoldable

sinkResponses :: Ordering -> Effect Unit
sinkResponses ord = do
  log (show ord)

sinkDone :: DecisionDisplays -> Effect Unit
sinkDone els = do
  log "done"
  J.hide els.scenario

sinkLottery ::
     DecisionDisplays
  -> Ratio String Number
  -> Effect Unit
sinkLottery els lottery = do
  log $ show lottery
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
     VisualizationDisplays
  -> Ref (Maybe Vega.View)
  -> UtilityFn String Number
  -> Effect Unit
sinkFunction els ref fn = do
  log $ show fn
  case (\best' -> Tuple best' <$> Map.lookup best' (byGood fn)) =<< best fn of
    Nothing -> clear
    Just (Tuple best' ratios) -> do
      let intervals :: forall f. Unfoldable f => Functor f => f ChartInterval
          intervals = toChartInterval best' <$> Set.toUnfoldable ratios
      maybe (createViewWithData best' intervals) (flip changeData intervals) =<< Ref.read ref
  where
    createViewWithData best' intervals = do
      J.displayOne els.caption
      width <- J.width els.functionVisualization
      id <- unsafeFromForeign <$> J.getProp "id" els.functionVisualization
      Vega.embed
        ("#" <> id)
        (Argo.fromObject $ Object.union (chartSpec width best') (chartData intervals))
        (Argo.fromObject chartOpts)
        (flip Ref.write ref <<< Just)

    changeData view intervals =
      Vega.changeData
            "main"
            (Both {remove: Vega.ByPredicate $ const true} {insert: extractRecord <<< encodeAsObject <$> intervals })
            view
    clear = do
      J.hideOne els.caption
      J.clearOne els.functionVisualization
      flip Ref.write ref Nothing
    encodeAsObject = unsafeFromJustBecause "Static object" <<< Argo.toObject <<< Argo.encodeJson
    toChartInterval best'
      r@(MkRatio
        { pair: MkPair {base, quote}
        , relativeValue: bounds
        }
      ) =
        case finite (unmake bounds).lower of
          Just (MkFinite { bound: lowerBound }) ->
            MkChartInterval $
            if best' == base
              then {good: quote, lower: 1.0 / upperBound, upper: 1.0 / lowerBound}
              -- We bump `lower` by smallest because log scale can't handle 0
              else {good: base, lower: lowerBound + smallest, upper: upperBound}
          Nothing -> unsafeCrashWith $ "Presence of 'best' implies these properties.\nr:" <> show r <> "\nbest': " <> show best'
        where
          upperBound = maybe VNM.top ((_.bound) <<< unwrap) <<< finite $ (unmake bounds).upper
