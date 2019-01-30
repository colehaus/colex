module Html where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafePartialBecause)

type RuleElements =
  { input :: JQuery (One "textarea")
  , output :: JQuery (One "div")
  }

type Elements =
  { maximinElements :: RuleElements
  , maximaxElements :: RuleElements
  , leximinElements :: RuleElements
  }

collectElements :: Effect Elements
collectElements =
  unsafePartialBecause "We require these elements to function" ado
    maximinElements <- ado
      Just input <- J.selectOne "#maximin-table"
      Just output <- J.selectOne "#maximin-analysis"
      in { input, output }
    maximaxElements <- ado
      Just input <- J.selectOne "#maximax-table"
      Just output <- J.selectOne "#maximax-analysis"
      in { input, output }
    leximinElements <- ado
      Just input <- J.selectOne "#leximin-table"
      Just output <- J.selectOne "#leximin-analysis"
      in { input, output }
    in { maximinElements, maximaxElements, leximinElements }

