module Html where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery.Fancy (JQuery, One)
import Control.Monad.Eff.JQuery.Fancy as J
import DOM (DOM)
import Data.Maybe (Maybe(..))
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

collectInputs :: forall e. Eff (dom :: DOM | e) Inputs
collectInputs = do
  unsafePartialBecause "We require this element structure to function" do
    Just stochasticMax <- J.selectOne "#stochastic-max"
    Just deterministicMax <- J.selectOne "#deterministic-max"
    Just numContestants <- J.selectOne "#num-contestants"
    pure { stochasticMax, deterministicMax, numContestants }

collectCharts :: forall e. Eff (dom :: DOM | e) Charts
collectCharts = do
  unsafePartialBecause "We require this element structure to function" do
    Just combinedChart <- J.selectOne "#combined-chart"
    pure { combinedChart }

collectOutputs :: forall e. Eff (dom :: DOM | e) Outputs
collectOutputs =
  unsafePartialBecause "We require this element structure to function" do
    Just probMax <- J.selectOne "#prob-max"
    Just meanStochastic <- J.selectOne "#mean-stochastic"
    pure { probMax, meanStochastic }

collectElements :: forall e. Eff (dom :: DOM | e) Elements
collectElements = do
  inputs <- collectInputs
  charts <- collectCharts
  outputs <- collectOutputs
  pure { inputs, charts, outputs }
