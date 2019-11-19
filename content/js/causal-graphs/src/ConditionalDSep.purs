-- Lots of duplication with `GraphToD` but that seems okay given circumstances.
module ConditionalDSep where

import Prelude

import App (App)
import Causal.Kernel (Path, disjointnessTwoSet)
import Color (rgb)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Functor.Compose (Compose(..))
import Data.Graph (Graph)
import Data.Graph.Causal (dSeparations)
import Data.Graph.Causal as Causal
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, uncurry4)
import Data.TwoSet (TwoSet(..))
import Data.Yaml (parseFromYaml)
import DotLang (graphToGraph, highlightPaths)
import Effect (Effect)
import FRP ((<+>))
import FRP.Event (Event)
import FRP.JQuery (inputTextChangeEvent, textAreaChangeEvent)
import GDP.Named (name3, unName)
import Graphics.Graphviz (Engine(..))
import Graphics.Graphviz as Dot
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Partial.Unsafe (unsafeCrashWith)
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
  , conditionedOn :: JQuery (One "textarea")
  , svg :: JQuery (One "div")
  , error :: JQuery (One "div")
  }

type DConnection =
  { from :: JQuery (One "input")
  , to :: JQuery (One "input")
  , result :: JQuery (One "div")
  }

type RawInput = Tuple4 String String String String

type Input k v =
  { graph :: Graph k v
  , connectionQuery :: Maybe (TwoSet k)
  , conditionedOn :: Set k
  }

type Analysis k v =
  { graph :: Graph k v
  , dConnections :: Maybe (Set (Path k))
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
  , parse: uncurry4 parse
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
      spec <- Compose $ J.selectOne "#graph-conditioned-spec"
      conditionedOn <- Compose $ J.selectOne "#graph-conditioned-on"
      svg <- Compose $ J.selectOne "#graph-svg"
      error <- Compose $ J.selectOne "#graph-error"
      in { spec, conditionedOn, svg, error }
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
  conditionedOn <- textAreaChangeEvent els.specAndRender.conditionedOn
  cf <- inputTextChangeEvent els.dConnection.from
  ct <- inputTextChangeEvent els.dConnection.to
  in
    spec <+> conditionedOn <+> cf <+> ct <+> (Tuple unit $ pure unit)

parse :: String -> String -> String -> String -> Either String (Input String String)
parse graphS conditionedOnS fromS toS = do
  graph <- stringToGraph graphS
  conditionedOn <- decodeJson <=< parseFromYaml $ conditionedOnS
  from <-
    traverse (conditionedVertex conditionedOn <=< missingVertex graph) $ emptyToNothing fromS
  to <-
    traverse (conditionedVertex conditionedOn <=< missingVertex graph) $ emptyToNothing toS
  pure $ { graph, conditionedOn, connectionQuery: MkTwoSet <$> from <*> to }
  where
    conditionedVertex s k
      | k `Set.member` s = Left "d-connection query asks about vertex in conditioning set"
      | otherwise = Right k
    missingVertex g k
      | vertexInGraph k g = Right k
      | otherwise = Left "d-connection query asks about vertex not in graph"
    emptyToNothing s
      | s == mempty = Nothing
      | otherwise = Just s

analyze :: forall k v. Ord k => Input k v -> Analysis k v
analyze { graph, connectionQuery, conditionedOn } =
  { graph
  , dSeparations: dSeparations conditionedOn graph
  , dConnections: dConnections' <$> connectionQuery
  }
  where
    dConnections' ks = name3 ks conditionedOn graph (\ks' conditionedOn' graph' ->
      case disjointnessTwoSet ks' conditionedOn' of
        Just proof ->
          Set.map unName $ Causal.dConnectedBy proof ks' conditionedOn' graph'
        Nothing ->
          -- TODO: It would be nice to actually create the proof there and
          -- carry it through to here instead of rechecking
          unsafeCrashWith "Already checked conditioning overlap during parsing")

unparse :: forall k v. Show k => (k -> String) -> (v -> String) -> Analysis k v -> Output
unparse kToId valueToLabel { dSeparations: dSep, dConnections, graph } =
  { svg, dConnections, dSeparations: dSeparationsEl }
  where
    dSeparationsEl =
      renderFoldableAsHtmlList Ul (MkElement "<div>None</div>") itemToHtml dSep
      where
        itemToHtml (MkTwoSet k1 k2) = kToId k1 <> " and " <> kToId k2
    Tuple dConnections svg =
      rmap (MkElement <<< Dot.renderToSvg Dot) <<< maybe nothing just $ dConnections
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
render els { dConnections, svg, dSeparations: dSep } =
  maybe (J.clearOne els.dConnection.result) (replaceElIn els.dConnection.result) dConnections *>
  replaceElIn els.specAndRender.svg svg *>
  replaceElIn els.dSeparationResults dSep
