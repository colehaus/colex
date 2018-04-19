module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery)
import Control.Monad.Eff.JQuery as J
import DOM (DOM)
import Data.Argonaut.Encode as Argonaut
import Data.Argonaut.Generic.Aeson as Generic
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.Foreign (unsafeFromForeign)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty.Indexed (NonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Number.Format (fixed, toStringWith)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Yaml (parseFromYaml, printToYaml)
import FRP (FRP)
import FRP.Event (Event)
import FRP.Event (create, fold, subscribe) as FRP
import Math.Probability.Dist (Dist)
import Math.Probability.Information.Value (Value)
import Math.Probability.Information.Value (evMaxDecide, evMaxValue, forget, investigationAndDecisionTreeValue, perfect, simpleDecisionTreeValue, valueOfInformation) as VOI
import Math.Probability.Information.Value.InvestigationAndDecisionTree (InvestigationAndDecisionTree)
import Math.Probability.Information.Value.InvestigationAndDecisionTree (fromElementary) as VOI
import Math.Probability.Information.Value.InvestigationAndDecisionTree as IDT
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (unsafeFromRightBecause)
import Math.Probability.Prob.Number (Prob(..))

import Data (Result(MkResult), first, unwrapInvestigation, wrapChoice, wrapInvestigation)


asList :: forall a. List a -> List a
asList = id

type Elements =
  { forgotten :: JQuery
  , perfected :: JQuery
  , expectedValue :: JQuery
  , forgottenValue :: JQuery
  , perfectedValue :: JQuery
  , expectedVoi :: JQuery
  , perfectedVoi :: JQuery
  , error :: JQuery
  }

type Inputs = { voiText :: Event String, initialVoiText :: String }

collectElements :: forall e. Eff (dom :: DOM | e) Elements
collectElements = do
  forgotten <- J.select "#forgotten"
  perfected <- J.select "#perfected"
  expectedValue <- J.select "#expected-value"
  forgottenValue <- J.select "#forgotten-expected-value"
  perfectedValue <- J.select "#perfected-expected-value"
  expectedVoi <- J.select "#voi-result"
  perfectedVoi <- J.select "#voi-perfect"
  error <- J.select "#voi-error"
  pure { forgotten, perfected, expectedValue, forgottenValue, perfectedValue, expectedVoi, perfectedVoi, error }

setup :: forall e. Eff (dom :: DOM, frp :: FRP | e) Inputs
setup = do
  voiTextEl <- J.select "#voi-text"
  voiText <- textAreaEvent voiTextEl
  initialVoiText <- J.getText voiTextEl
  pure { voiText, initialVoiText }

sink :: forall e. Elements -> Either String Unit -> InvestigationAndDecisionTree Prob String String (Result String Number) -> Eff (dom :: DOM | e) Unit
sink el error tree = do
  void $ errorTextEl el.error error
  void $ J.setText (show' <<< untagged <<< voi $ tree) el.expectedVoi
  void $ J.setText (show' <<< untagged <<< voi $ perfected) el.perfectedVoi
  void $ J.setText (show' <<< untagged <<< simpleDecisionTreeValue $ forgotten) el.forgottenValue
  void $ J.setText (show' <<< untagged <<< investigationAndDecisionTreeValue $ tree) el.expectedValue
  void $ J.setText (show' <<< untagged <<< investigationAndDecisionTreeValue $ perfected) el.perfectedValue
  void $ J.setText (printSimpleDecisionTree forgotten) el.forgotten
  void $ J.setText (printInvestigationAndDecisionTree perfected) el.perfected
  where
    show' = toStringWith (fixed 2)
    forgotten = VOI.forget tree
    perfected = VOI.perfect <<< VOI.forget $ tree

main :: forall e. Eff (dom :: DOM, frp :: FRP | e) Unit
main =
  J.ready do
    inputs <- setup
    let initialTree = unsafeFromRightBecause "Starting text known good" $ parse inputs.initialVoiText
    elements <- collectElements
    sink elements (Right unit) initialTree
    FRP.subscribe (latestSuccess parse initialTree inputs.voiText) (uncurry $ sink elements)

printInvestigationAndDecisionTree ::
     InvestigationAndDecisionTree
       Prob
       (NonEmpty Map String (Result String Number))
       String
       (Result String Number)
  -> String
printInvestigationAndDecisionTree =
  printToYaml <<<
  Generic.encodeJson <<< wrapInvestigation <<< IDT.elementary <<< asStrMaps
  where
    -- No generic encoder for `Map`s so we convert it to a string first
    asStrMaps =
      unsafeFromRightBecause "encode and decode should be isomorphic" <<<
      IDT.lift
        (map <<< first $
         printToYaml <<<
         Argonaut.encodeJson <<<
         StrMap.fromFoldable <<<
         asList <<< Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert)

printSimpleDecisionTree ::
     SimpleDecisionTree Prob String (Result String Number) -> String
printSimpleDecisionTree =
  printToYaml <<<
  Generic.encodeJson <<<
  Array.fromFoldable <<< map wrapChoice <<< SDT.elementary

simpleDecisionTreeValue ::
     SimpleDecisionTree Prob String (Result String Number) -> Tagged Value Number
simpleDecisionTreeValue = VOI.simpleDecisionTreeValue resultToValue evMaxDecide evMaxValue

investigationAndDecisionTreeValue ::
     forall f.
     Ord f
  => InvestigationAndDecisionTree Prob f String (Result String Number)
  -> Tagged Value Number
investigationAndDecisionTreeValue =
  VOI.investigationAndDecisionTreeValue resultToValue evMaxDecide evMaxValue

evMaxDecide :: SimpleDecisionTree Prob String (Result String Number) -> String
evMaxDecide = VOI.evMaxDecide resultToValue probToValue

evMaxValue :: Dist Prob (Tagged Value Number) -> Tagged Value Number
evMaxValue = VOI.evMaxValue probToValue

resultToValue :: forall a b. Result a b -> Tagged Value b
resultToValue (MkResult r) = tagged r.value

probToValue :: Prob -> Tagged Value Number
probToValue (MkProb p) = tagged p

voi ::
     forall a.
     Ord a
  => InvestigationAndDecisionTree Prob a String (Result String Number)
  -> Tagged Value Number
voi =
  VOI.valueOfInformation
    resultToValue
    evMaxDecide
    evMaxValue

parse ::
     String
  -> Either String (InvestigationAndDecisionTree Prob String String (Result String Number))
parse =
  lmap show <<<
  VOI.fromElementary <<<
  unwrapInvestigation <=< Generic.decodeJson <=< parseFromYaml

errorTextEl :: forall a e. JQuery -> Either String a -> Eff ( dom :: DOM | e) Unit
errorTextEl el = either (\t -> J.setText t el *> J.display el) (\_ -> J.hide el)

latestSuccess ::
     forall i o e.
     (i -> Either e o)
  -> o
  -> Event i
  -> Event (Tuple (Either e Unit) o)
latestSuccess g o evt = FRP.fold f evt (Tuple (Right unit) o)
  where
    f string (Tuple _ old) =
      case g string of
        Right new -> Tuple (Right unit) new
        Left e -> Tuple (Left e) old

jqueryEvent ::
     forall a e.
     String
  -> (Unit -> Eff (frp :: FRP, dom :: DOM | e) a)
  -> JQuery
  -> Eff (dom :: DOM , frp :: FRP | e) (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\_ _ -> push =<< f unit) el
  pure event

textAreaEvent :: forall e. JQuery -> Eff ( dom :: DOM , frp :: FRP | e) (Event String)
textAreaEvent el =
  jqueryEvent "input" (\_ -> unsafeFromForeign <$> J.getValue el) el
