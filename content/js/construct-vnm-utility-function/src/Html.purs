module Html where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery.Fancy (JQuery, One)
import Control.Monad.Eff.JQuery.Fancy as J
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartialBecause)

type DecisionDisplays =
  { firstChance :: JQuery (One "span")
  , secondChance :: JQuery (One "span")
  , firstGood :: JQuery (One "span")
  , secondGood :: JQuery (One "span")
  }

type VisualizationDisplays =
  { functionVisualization :: JQuery (One "div")
  , caption :: JQuery (One "figcaption")
  }

type InitialInputs =
  { goods :: JQuery (One "textarea")
  }

type DecisionInputs =
  { first :: JQuery (One "span")
  , indifferent :: JQuery (One "span")
  , second :: JQuery (One "span")
  }

type Elements =
  { initialInputs :: InitialInputs
  , decisionInputs :: DecisionInputs
  , decisionDisplays :: DecisionDisplays
  , visualizationDisplays :: VisualizationDisplays
  }

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
    Just functionVisualization <- J.selectOne "#function-chart"
    Just caption <- J.selectOne "#function-visualization figcaption"
    pure { functionVisualization, caption }
