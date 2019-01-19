module Html where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafePartialBecause)

type Elements =
  { input :: JQuery (One "textarea")
  , output :: JQuery (One "div")
  }

collectElements :: Effect Elements
collectElements = do
  unsafePartialBecause "We require these elements to function" do
    Just input <- J.selectOne "#decision-table"
    Just output <- J.selectOne "#decision-analysis"
    pure { input, output }
