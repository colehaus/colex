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

type Elements =
  { maximinElements :: Maybe RuleElements
  , maximaxElements :: Maybe RuleElements
  , leximinElements :: Maybe RuleElements
  , strongDominanceElements :: Maybe RuleElements
  , weakDominanceElements :: Maybe RuleElements
  }

collectElements :: Effect Elements
collectElements = ado
  maximinElements <- sequence' <$> ado
    input <- J.selectOne "#maximin-table"
    output <- J.selectOne "#maximin-analysis"
    in { input, output }
  maximaxElements <- sequence' <$> ado
    input <- J.selectOne "#maximax-table"
    output <- J.selectOne "#maximax-analysis"
    in { input, output }
  leximinElements <- sequence' <$> ado
    input <- J.selectOne "#leximin-table"
    output <- J.selectOne "#leximin-analysis"
    in { input, output }
  strongDominanceElements <- sequence' <$> ado
    input <- J.selectOne "#dominance-table"
    output <- J.selectOne "#strong-dominance-analysis"
    in { input, output }
  weakDominanceElements <- sequence' <$> ado
    input <- J.selectOne "#dominance-table"
    output <- J.selectOne "#weak-dominance-analysis"
    in { input, output }
  in { maximinElements, maximaxElements, leximinElements, strongDominanceElements, weakDominanceElements }
  where
    sequence' { input: mInput, output: mOutput } = ado
      input <- mInput
      output <- mOutput
      in { input, output}

