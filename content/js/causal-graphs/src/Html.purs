module Html where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafePartialBecause)


type Elements =
  { graphElement :: JQuery (One "div")
  }

collectElements :: Effect Elements
collectElements = ado
  graphElement <- unsafePartialBecause "Element required" $ fromJust <$> J.selectOne "#graph"
  in { graphElement }
