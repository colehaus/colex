module DToGraph where

import Prelude

import App (App)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either)
import Data.Foldable (foldMap, traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Graph.Causal (dSeparations)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple as Tuple
import Data.TwoSet (TwoSet)
import Data.TwoSet as TwoSet
import Data.Yaml (parseFromYaml)
import DotLang (graphToGraph)
import Effect (Effect)
import FRP as FRP
import FRP.Event (Event)
import FRP.JQuery (textAreaChangeEvent)
import Graphics.Graphviz (Engine(..))
import Graphics.Graphviz as Dot
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Utility (distinctPairs, powerSet)
import Utility.Render (Element(..), replaceElIn)
import Utility.Render as Render

type Elements =
  { nodes :: JQuery (One "textarea")
  , dSeparations :: JQuery (One "textarea")
  , compatibleGraphs :: JQuery (One "div")
  , error :: JQuery (One "div")
  }

type RawInput = Tuple String String

type Input k =
  { nodes :: Set k
  , dSeparations :: Set (TwoSet k)
  }

type Analysis k =
  { compatibleGraphs :: List (Graph k k)
  }

elements :: Effect (Maybe Elements)
elements = un Compose $ ado
  dSeparations <- Compose $ J.selectOne "#d-separations"
  compatibleGraphs <- Compose $ J.selectOne "#compatible-graphs"
  error <- Compose $ J.selectOne "#d-separation-error"
  nodes <- Compose $ J.selectOne "#nodes"
  in { dSeparations, compatibleGraphs, error, nodes }

app :: App Elements RawInput String (Input String) (Analysis String) Element
app =
  { elements
  , readInput
  , parse: uncurry parse
  , analyze
  , unparse: unparse identity
  , render: \el -> traverse_ (render el)
  , error
  }

error :: Elements -> Maybe String -> Effect Unit
error els = Render.error els.error

readInput :: Elements -> Effect (Tuple RawInput (Event RawInput))
readInput els =
  FRP.combine <$> textAreaChangeEvent els.nodes <*> textAreaChangeEvent els.dSeparations

parse :: forall k. Ord k => DecodeJson k => String -> String -> Either String (Input k)
parse nodesS dSeparationsS = ado
    dSeparations <-
      map (Set.map TwoSet.fromTuple) <<< decodeJson <=< parseFromYaml $ dSeparationsS
    nodes <- decodeJson <=< parseFromYaml $ nodesS
  in { dSeparations, nodes }

analyze :: forall k. Ord k => Input k -> Analysis k
analyze { nodes, dSeparations: dSep } =
  { compatibleGraphs:
      -- Splitting the two filters avoid a stack overflow for some reason
      List.filter matchingDSeps <<< List.filter (nonCyclic && fullSize) <<<
      allGraphsExcept dSep $ nodes
  }
  where
    matchingDSeps g = dSep == dSeparations g
    nonCyclic = not <<< Graph.isCyclic
    fullSize g = Set.size nodes == (Set.size <<< Map.keys <<< Graph.toMap $ g)

-- `List` instead of `Set` because `Graph` isn't `Ord`
allGraphsExcept :: forall k. Ord k => Set (TwoSet k) -> Set k -> List (Graph k k)
allGraphsExcept dSeps nodes =
  map mkGraph <<< List.fromFoldable <<< Set.filter (not <<< Set.isEmpty) <<<
  powerSet <<< flip Set.difference dSeps' <<< distinctPairs $ nodes
  where
    dSeps' =
      Set.map TwoSet.toTuple dSeps <> Set.map (Tuple.swap <<< TwoSet.toTuple) dSeps
    mkGraph =
      Graph.fromMap <<< flip Map.union disconnectedGraph <<< Map.fromFoldableWith merge <<<
      Set.map (\(Tuple k1 k2) -> Tuple k1 <<< Tuple k1 $ Set.singleton k2)
    merge (Tuple k es1) (Tuple _ es2) = Tuple k $ es1 <> es2
    disconnectedGraph = Map.fromFoldable <<< Set.map (\k -> Tuple k (Tuple k Set.empty)) $ nodes

unparse :: forall k. Show k => (k -> String) -> Analysis k -> Element
unparse kToLabel { compatibleGraphs } =
  MkElement <<< foldMap (Dot.renderToSvg Dot <<< graphToGraph kToLabel) $ compatibleGraphs

render :: Elements -> Element -> Effect Unit
render els svgs = replaceElIn els.compatibleGraphs svgs
