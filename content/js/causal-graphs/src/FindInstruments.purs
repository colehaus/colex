module FindInstruments where

import Prelude

import App (App)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Graph (Graph)
import Data.Graph.Causal as Casual
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, uncurry3)
import DotLang (graphToGraph)
import Effect (Effect)
import FRP ((<+>))
import FRP.Event (Event)
import FRP.JQuery (inputTextChangeEvent, textAreaChangeEvent)
import Graphics.Graphviz (Engine(..), renderToSvg)
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Utility (stringToGraph, vertexInGraph)
import Utility.Render (Element(..), ListType(..), renderFoldableAsHtmlList, replaceElIn)
import Utility.Render as Render

type Elements =
  { specAndRender :: SpecAndRender
  , instruments :: Instruments
  }

type SpecAndRender =
  { spec :: JQuery (One "textarea")
  , svg :: JQuery (One "div")
  , error :: JQuery (One "div")
  }

type Instruments =
  { cause :: JQuery (One "input") -- Find instruments for this cause
  , effect :: JQuery (One "input") -- and this effect
  , result :: JQuery (One "div")
  }

type RawInput = Tuple3 String String String

type Input k v =
  { graph :: Graph k v
  , instrumentsQuery :: { cause :: k, effect :: k}
  }

type Analysis k v =
  { graph :: Graph k v
  , instruments :: Set k
  }

type Output =
  { svg :: Element
  , instrumentsList :: Element
  }

app :: App Elements RawInput String (Input String String) (Analysis String String) Output
app =
  { elements
  , readInput
  , parse: uncurry3 parse
  , analyze
  , unparse: unparse identity identity
  , render: \el -> traverse_ (render el)
  , error
  }

elements :: Effect (Maybe Elements)
elements =
  un Compose $ ado
    instruments <- Compose collectInstruments
    specAndRender <- Compose collectSpecAndRender
  in { instruments, specAndRender }
  where
    collectSpecAndRender = un Compose $ ado
      spec <- Compose $ J.selectOne "#graph-spec"
      svg <- Compose $ J.selectOne "#graph-svg"
      error <- Compose $ J.selectOne "#graph-error"
      in { spec, svg, error }
    collectInstruments = un Compose $ ado
      cause <- Compose $ J.selectOne "#instruments-cause"
      effect <- Compose $ J.selectOne "#instruments-effect"
      result <- Compose $ J.selectOne "#instruments-result"
    in { cause, effect, result }

readInput :: Elements -> Effect (Tuple RawInput (Event RawInput))
readInput els = ado
  spec <- textAreaChangeEvent els.specAndRender.spec
  instrumentsCause <- inputTextChangeEvent els.instruments.cause
  instrumentsEffect <- inputTextChangeEvent els.instruments.effect
  in
    spec <+> instrumentsCause <+> instrumentsEffect <+> (Tuple unit $ pure unit)

parse :: String -> String -> String -> Either String (Input String String)
parse graphS causeS effectS = do
  graph <- stringToGraph graphS
  cause <- missingVertex graph <=< emptyToLeft $ causeS
  effect <- missingVertex graph <=< emptyToLeft $ effectS
  pure $ { instrumentsQuery: { cause, effect }, graph }
  where
    missingVertex g k
      | vertexInGraph k g = Right k
      | otherwise = Left "Instruments query asks about vertex not in graph"
    emptyToLeft s
      | s == mempty = Left "Empty string in instruments query"
      | otherwise = Right s


analyze :: forall k v. Ord k => Input k v -> Analysis k v
analyze { graph, instrumentsQuery } =
  { graph, instruments: Casual.instruments instrumentsQuery Set.empty graph }

unparse :: forall k v. Show k => (k -> String) -> (v -> String) -> Analysis k v -> Output
unparse kToId valueToLabel { graph, instruments } = { svg, instrumentsList }
  where
    svg = MkElement <<< renderToSvg Dot <<< graphToGraph valueToLabel $ graph
    instrumentsList =
      renderFoldableAsHtmlList Ul (MkElement "<div>None</div>") itemToHtml instruments
      where
        itemToHtml k = kToId k

render :: Elements -> Output -> Effect Unit
render els { svg, instrumentsList } =
  replaceElIn els.specAndRender.svg svg *>
  replaceElIn els.instruments.result instrumentsList

error :: Elements -> Maybe String -> Effect Unit
error els = Render.error els.specAndRender.error

