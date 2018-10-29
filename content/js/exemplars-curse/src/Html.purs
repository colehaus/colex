module Html where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafePartialBecause)

type Elements =
  { inputs :: Inputs
  , outputs :: Outputs
  , charts :: Charts
  }

type Inputs =
  { stochasticMax :: JQuery (One "input")
  , deterministicMax :: JQuery (One "input")
  , numContestants :: JQuery (One "input")
  }

type Charts =
  { combinedChart :: JQuery (One "div")
  }

type Outputs =
  { probMax :: JQuery (One "span")
  , meanStochastic :: JQuery (One "span")
  }

collectInputs :: Effect Inputs
collectInputs = do
  unsafePartialBecause "We require this element structure to function" do
    Just stochasticMax <- J.selectOne "#stochastic-max"
    Just deterministicMax <- J.selectOne "#deterministic-max"
    Just numContestants <- J.selectOne "#num-contestants"
    pure { stochasticMax, deterministicMax, numContestants }

collectCharts :: Effect Charts
collectCharts = do
  unsafePartialBecause "We require this element structure to function" do
    Just combinedChart <- J.selectOne "#combined-chart"
    pure { combinedChart }

collectOutputs :: Effect Outputs
collectOutputs =
  unsafePartialBecause "We require this element structure to function" do
    Just probMax <- J.selectOne "#prob-max"
    Just meanStochastic <- J.selectOne "#mean-stochastic"
    pure { probMax, meanStochastic }

collectElements :: Effect Elements
collectElements = do
  inputs <- collectInputs
  charts <- collectCharts
  outputs <- collectOutputs
  pure { inputs, charts, outputs }
