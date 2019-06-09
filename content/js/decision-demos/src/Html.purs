module Html where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J

type RuleElements =
  { input :: JQuery (One "textarea")
  , output :: JQuery (One "div")
  }

type OptimPessimElements =
  { input :: JQuery (One "textarea")
  , alpha :: JQuery (One "input")
  , output :: JQuery (One "div")
  }

type Elements =
  { maximinElements :: Maybe RuleElements
  , maximaxElements :: Maybe RuleElements
  , leximinElements :: Maybe RuleElements
  , strongDominanceElements :: Maybe RuleElements
  , weakDominanceElements :: Maybe RuleElements
  , optimismPessimismElements :: Maybe OptimPessimElements
  , minimaxRegretElements :: Maybe RuleElements
  , indifferenceElements :: Maybe RuleElements
  }

collectElements :: Effect Elements
collectElements = ado
  maximinElements <- collectElementsForRule "maximin"
  maximaxElements <- collectElementsForRule "maximax"
  leximinElements <- collectElementsForRule "leximin"
  indifferenceElements <- collectElementsForRule "indifference"
  minimaxRegretElements <- collectElementsForRule "minimax-regret"
  strongDominanceElements <- sequence' <$> ado
    input <- J.selectOne "#dominance-table"
    output <- J.selectOne "#strong-dominance-analysis"
    in { input, output }
  weakDominanceElements <- sequence' <$> ado
    input <- J.selectOne "#dominance-table"
    output <- J.selectOne "#weak-dominance-analysis"
    in { input, output }
  optimismPessimismElements <- sequence'' <$> ado
    input <- J.selectOne "#optimism-pessimism-table"
    alpha <- J.selectOne "#optimism-pessimism-alpha"
    output <- J.selectOne "#optimism-pessimism-analysis"
    in { input, alpha, output}
  in
    { maximinElements
    , maximaxElements
    , leximinElements
    , strongDominanceElements
    , weakDominanceElements
    , optimismPessimismElements
    , minimaxRegretElements
    , indifferenceElements
    }
  where
    collectElementsForRule name =
      sequence' <$> ado
        input <- J.selectOne $ "#" <> name <> "-table"
        output <- J.selectOne $ "#" <> name <> "-analysis"
        in { input, output }
    sequence' { input: mInput, output: mOutput } = ado
      input <- mInput
      output <- mOutput
      in { input, output }
    sequence'' { input: mInput, output: mOutput, alpha: mAlpha } = ado
      input <- mInput
      output <- mOutput
      alpha <- mAlpha
      in { input, output, alpha }


