module Discriminate where

import Prelude

import App (App)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, traverse_)
import Data.Function (on)
import Data.Functor.Compose (Compose(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Graph.Causal as Causal
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.TwoSet (TwoSet)
import DotLang (graphToGraph)
import Effect (Effect)
import FRP as FRP
import FRP.Event (Event)
import FRP.JQuery (textAreaChangeEvent)
import Graphics.Graphviz (Engine(..))
import Graphics.Graphviz as Dot
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy as J
import Utility (stringToGraph)
import Utility.Render (Element(..), replaceElIn)
import Utility.Render as Render

type Elements =
  { spec1 :: JQuery (One "textarea")
  , spec2 :: JQuery (One "textarea")
  , error :: JQuery (One "div")
  , analysis :: JQuery (One "div")
  }

type RawInput = Tuple String String

type Input k v =
  { graph1 :: Graph k v
  , graph2 :: Graph k v
  }

data DiscriminationStatus = Discrimination | NoDiscrimination
data DStatus = DConnected | DSeparated

type Instrument k v =
  { instrument :: k
  , cause :: k
  , graph1 :: Tuple (Set k) (Graph k v)
  , graph2 :: Tuple (Set k) (Graph k v)
  }

type Analysis k v =
  { instruments ::
       Either
         { graph1 :: Set (TwoSet k), graph2 :: Set (TwoSet k) }
         (Array (Instrument k v))
  }

type Output =
  { analysis :: Element
  }


app :: App Elements RawInput String (Input String String) (Analysis String String) Output
app =
  { elements
  , readInput
  , parse: uncurry parse
  , analyze: analyze (_ <> "-instrument") (_ <> "-instrument")
  , unparse: unparse identity identity
  , render: \el -> traverse_ (render el)
  , error
  }

elements :: Effect (Maybe Elements)
elements = un Compose $ ado
  spec1 <- Compose $ J.selectOne "#graph-spec1"
  spec2 <- Compose $ J.selectOne "#graph-spec2"
  error <- Compose $ J.selectOne "#discriminate-error"
  analysis <- Compose $ J.selectOne "#discriminate-analysis"
  in { spec1 , spec2, error, analysis }

readInput :: Elements -> Effect (Tuple RawInput (Event RawInput))
readInput els = FRP.combine <$> textAreaChangeEvent els.spec1 <*> textAreaChangeEvent els.spec2

parse :: String -> String -> Either String (Input String String)
parse graph1S graph2S = do
  graph1 <- stringToGraph graph1S
  graph2 <- stringToGraph graph2S
  if Map.keys (Graph.toMap graph1) == Map.keys (Graph.toMap graph2)
     then pure unit
     else Left "This only makes sense for graphs with the same vertices"
  pure $ { graph1,  graph2 }

analyze :: forall k v. Ord k => (k -> k) -> (v -> v) -> Input k v -> Analysis k v
analyze idForInstrument valueForInstrument { graph1, graph2 } =
  case Tuple (Causal.dSeparations graph1) (Causal.dSeparations graph2) of
    Tuple graph1Seps graph2Seps
      | graph1Seps == graph2Seps ->
        { instruments: _ } <<< Right <<< map munge <<<
        Array.filter (uncurry $ (/=) `on` _.dSeparations) $
        Array.zip (mkVariations graph1) (mkVariations graph2)
      | otherwise -> { instruments: Left { graph1: graph1Seps, graph2: graph2Seps } }
  where
    munge (Tuple graph1Variant graph2Variant) =
      { instrument: idForInstrument graph1Variant.cause
      , cause: graph1Variant.cause
      , graph1: Tuple graph1Variant.dSeparations graph1Variant.graph
      , graph2: Tuple graph2Variant.dSeparations graph2Variant.graph
      }
    mkVariations g = Array.zipWith dSeps vertices (addInstrument g <$> vertices)
      where
        dSeps (Tuple k _) g' =
          { cause: k
          , dSeparations: (Causal.dSeparatedFrom (idForInstrument k) Set.empty g')
          , graph: g'
          }
        -- We already checked during the parse phase that the graphs have the same vertices
        vertices = Map.toUnfoldable <<< map fst <<< Graph.toMap $ graph1
        addInstrument graph (Tuple k v) =
          Graph.insertEdgeWithVertices
            (Tuple (idForInstrument k) (valueForInstrument v))
            (Tuple k v)
            graph

unparse :: forall k v. Show k => (k -> String) -> (v -> String) -> Analysis k v -> Output
unparse idToString valueToLabel =
  { analysis: _  } <<<
  either noInstruments (MkElement <<< foldMap unparseInstrument) <<<
  _.instruments
  where
    noInstruments { graph1, graph2 } =
      MkElement "<div>These two graphs are already distinguishable!</div>"
    unparseInstrument { instrument, cause, graph1, graph2 } =
      renderGraph (snd graph1) <> renderGraph (snd graph2) <> text
      where
        text =
          "<div> Instrument " <>
          idToString instrument <>
          " and cause " <>
          idToString cause <>
          "</div>"
        renderGraph = Dot.renderToSvg Dot <<< graphToGraph valueToLabel

render :: Elements -> Output -> Effect Unit
render els = replaceElIn els.analysis <<< _.analysis

error :: Elements -> Maybe String -> Effect Unit
error els = Render.error els.error
