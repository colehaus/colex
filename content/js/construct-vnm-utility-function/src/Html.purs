module Html where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import JQuery.Fancy (JQuery, Many, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafePartialBecause)

type DecisionDisplays =
  { scenario :: JQuery Many
  , firstChance :: JQuery (One "span")
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

collectElements :: Effect Elements
collectElements = do
  initialInputs <- collectInitialInputs
  decisionInputs <- collectDecisionInputs
  decisionDisplays <- collectDecisionDisplays
  visualizationDisplays <- collectVisualizationDisplays
  pure { initialInputs, decisionInputs, decisionDisplays, visualizationDisplays }

collectInitialInputs :: Effect InitialInputs
collectInitialInputs =
  unsafePartialBecause "We require this element structure to function" do
    Just goods <- J.selectOne "#goods"
    pure { goods }

collectDecisionInputs :: Effect DecisionInputs
collectDecisionInputs =
  unsafePartialBecause "We require this element structure to function" do
    Just first <- J.selectOne "#first-good"
    Just indifferent <- J.selectOne "#indifferent"
    Just second <- J.selectOne "#second-good"
    pure { first, indifferent, second }

collectDecisionDisplays :: Effect DecisionDisplays
collectDecisionDisplays =
  unsafePartialBecause "We require this element structure to function" do
    Just scenario <- J.select ".input"
    [firstChance, secondChance] <- J.selectOnes ".odds"
    [firstGood, secondGood] <- J.selectOnes ".good"
    pure { scenario, firstChance, secondChance, firstGood, secondGood }

collectVisualizationDisplays :: Effect VisualizationDisplays
collectVisualizationDisplays =
  unsafePartialBecause "We require this element structure to function" do
    Just functionVisualization <- J.selectOne "#function-chart"
    Just caption <- J.selectOne "#function-visualization figcaption"
    pure { functionVisualization, caption }
