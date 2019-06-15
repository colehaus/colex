module GraphToD where

import Prelude

import App (App)
import Color (rgb)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Functor.Compose (Compose(..))
import Data.Graph (Graph)
import Data.Graph.Causal (dSeparations)
import Data.Graph.Causal as Causal
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, uncurry3)
import Data.TwoSet (TwoSet(..))
import DotLang (graphToGraph, highlightPaths)
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
  , dSeparationResults :: JQuery (One "div")
  , dConnection :: DConnection
  }

type SpecAndRender =
  { spec :: JQuery (One "textarea")
  , svg :: JQuery (One "div")
  , error :: JQuery (One "div")
  }

type DConnection =
  { from :: JQuery (One "input")
  , to :: JQuery (One "input")
  , result :: JQuery (One "div")
  }

type RawInput = Tuple3 String String String

type Input k v =
  { graph :: Graph k v
  , connectionQuery :: Maybe (TwoSet k)
  }

type Analysis k v =
  { graph :: Graph k v
  , dConnections :: Maybe (Set (List k))
  , dSeparations :: Set (TwoSet k)
  }

type Output =
  { svg :: Element
  , dConnections :: Maybe Element
  , dSeparations :: Element
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
    dConnection <- Compose collectDConnection
    specAndRender <- Compose collectSpecAndRender
    dSeparationResults <- Compose $ J.selectOne "#d-separation-results"
    in { dConnection, specAndRender, dSeparationResults }
  where
    collectSpecAndRender = un Compose $ ado
      spec <- Compose $ J.selectOne "#graph-spec"
      svg <- Compose $ J.selectOne "#graph-svg"
      error <- Compose $ J.selectOne "#graph-error"
      in { spec, svg, error }
    collectDConnection = un Compose $ ado
      from <- Compose $ J.selectOne "#d-connection-from"
      to <- Compose $ J.selectOne "#d-connection-to"
      result <- Compose $ J.selectOne "#d-connection-result"
      in { from, to, result }

error :: Elements -> Maybe String -> Effect Unit
error els = Render.error els.specAndRender.error

readInput :: Elements -> Effect (Tuple RawInput (Event RawInput))
readInput els = ado
  spec <- textAreaChangeEvent els.specAndRender.spec
  cf <- inputTextChangeEvent els.dConnection.from
  ct <- inputTextChangeEvent els.dConnection.to
  in
    spec <+> cf <+> ct <+> (Tuple unit $ pure unit)

unparse :: forall k v. Show k => (k -> String) -> (v -> String) -> Analysis k v -> Output
unparse kToId valueToLabel { dSeparations: dSep, dConnections, graph } =
  { svg, dConnections, dSeparations: dSeparationsEl }
  where
    dSeparationsEl =
      renderFoldableAsHtmlList Ul (MkElement "<div>None</div>") itemToHtml dSep
      where
        itemToHtml (MkTwoSet k1 k2) = kToId k1 <> " and " <> kToId k2
    Tuple dConnections svg =
      rmap (MkElement <<< renderToSvg Dot) <<< maybe nothing just $ dConnections
        where
          dotGraph = graphToGraph valueToLabel graph
          nothing = Tuple Nothing dotGraph
          just cons =
            Tuple
              (Just $ renderDConnections cons)
              (highlightPaths bodyBackAltColor cons dotGraph)
            where
              bodyBackAltColor = rgb 181 187 230
              renderDConnections set =
                renderFoldableAsHtmlList Ul (MkElement "<div>They aren't</div>") itemToHtml set
                where
                  itemToHtml path = Foldable.intercalate "→" (kToId <$> path)

render :: Elements -> Output -> Effect Unit
render els { dConnections, svg, dSeparations: dSep } = ado
  maybe (J.clearOne els.dConnection.result) (replaceElIn els.dConnection.result) dConnections
  replaceElIn els.specAndRender.svg svg
  replaceElIn els.dSeparationResults dSep
  in unit

analyze :: forall k v. Ord k => Input k v -> Analysis k v
analyze { graph, connectionQuery } =
  { graph
  , dSeparations: dSeparations graph
  , dConnections:
    (\ks -> Causal.dConnectedBy ks Set.empty graph) <$> connectionQuery
  }

parse :: String -> String -> String -> Either String (Input String String)
parse graphS fromS toS = do
  graph <- stringToGraph graphS
  from <- traverse (missingVertex graph) $ emptyToNothing fromS
  to <- traverse (missingVertex graph) $ emptyToNothing toS
  pure $ { graph, connectionQuery: MkTwoSet <$> from <*> to }
  where
    missingVertex g k
      | vertexInGraph k g = Right k
      | otherwise = Left "d-connection query asks about vertex not in graph"
    emptyToNothing s
      | s == mempty = Nothing
      | otherwise = Just s
