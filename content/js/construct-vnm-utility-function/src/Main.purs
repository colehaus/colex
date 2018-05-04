module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.JQuery (ready) as J
import Control.Monad.Eff.JQuery.Fancy (JQuery, One)
import Control.Monad.Eff.JQuery.Fancy (getText, getValue, on, selectOne, selectOnes, setText) as J
import DOM (DOM)
import DOM.Event.Types (EventType)
import Data.Either (Either(..), either, hush, note)
import Data.Filterable (filter)
import Data.Foreign (unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Number.Format (exponential, toStringWith)
import Data.Set as Set
import Data.String as String
import Economics.Utility.Ratio (Ratio)
import Economics.Utility.Ratio as Ratio
import Economics.Utility.VNM (nonEmptySet, pickNextLottery, refine)
import Economics.Utility.VNM.Function (UtilityFn, goodsToInitialFn)
import FRP (FRP)
import FRP.Event (Event)
import FRP.Event as FRP
import Partial.Unsafe (unsafePartialBecause)

main :: forall e. Eff (dom :: DOM, frp :: FRP, console :: CONSOLE | e) Unit
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
    void $
      FRP.subscribe function (sinkFunction elements . visualizationDisplays)

update ::
     forall a.
     Ord a
  => Either (UtilityFn a Number) Ordering
  -> UtilityFn a Number
  -> UtilityFn a Number
update evt oldFn = either id (flip (refine (pickNextLottery oldFn)) oldFn) evt

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
    Just first <- J.selectOne "#first"
    Just indifferent <- J.selectOne "#indifferent"
    Just second <- J.selectOne "#second"
    pure { first, indifferent, second }

response ::
     forall e.
     DecisionInputs
  -> Eff (dom :: DOM, frp :: FRP, console :: CONSOLE | e) (Event Ordering)
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
  -> Eff (dom :: DOM, console :: CONSOLE | e) Unit
sinkLottery els lottery = do
  J.setText (Ratio.base lottery) els.firstGood
  J.setText (Ratio.quote lottery) els.secondGood
  J.setText (show' 1.0) els.firstChance
  J.setText (show' (unwrap lottery).relativeValue) els.secondChance
  where
    show' = toStringWith (exponential 2)

sinkFunction ::
     forall e.
     VisualizationDisplays
  -> UtilityFn String Number
  -> Eff (dom :: DOM, console :: CONSOLE | e) Unit
sinkFunction els map = do
  J.setText (show map) els.functionVisualization

jqueryEvent ::
     forall a e tag.
     EventType
  -> (Unit -> Eff (frp :: FRP, dom :: DOM | e) a)
  -> JQuery (One tag)
  -> Eff (dom :: DOM , frp :: FRP | e) (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\_ _ -> push =<< f unit) el
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

