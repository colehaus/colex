module Html where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafePartialBecause)


type Elements =
  { graphInput :: JQuery (One "textarea")
  , graphOutput :: JQuery (One "div")
  }

collectElements :: Effect Elements
collectElements = ado
  graphInput <- unsafePartialBecause "Element required" $ fromJust <$> J.selectOne "#graph-input"
  graphOutput <- unsafePartialBecause "Element required" $ fromJust <$> J.selectOne "#graph-output"
  in { graphInput, graphOutput }
