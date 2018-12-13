module Main where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Newtype (unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Yaml (parseFromYaml, printToYaml)
import Effect (Effect)
import Effect.Console as Console
import Foreign (Foreign, unsafeFromForeign)
import FRP.Event (Event)
import FRP.Event (create, fold, subscribe) as FRP
import JQuery (Selector)
import JQuery (display, hide, ready) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (getText, getValue, on, setText, unsafeSelectOneBecause) as J
import Math.Probability.Dist (Dist)
import Math.Probability.Information.Value (Value)
import Math.Probability.Information.Value (evMaxDecide, evMaxValue, forget, investigationAndDecisionTreeValue, simpleDecisionTreeValue, valueOfInformation) as VOI
import Math.Probability.Information.Value.InvestigationAndDecisionTree (InvestigationAndDecisionTree)
import Math.Probability.Information.Value.InvestigationAndDecisionTree (fromElementary) as VOI
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (unsafeFromRightBecause)
import Math.Probability.Prob.Number (Prob(..))
import Web.Event.Event (EventType(..))

import Data (Result(MkResult), encodeChoiceJson, unwrapInvestigation, wrapChoice, decodeInvestigationJson)

foreign import jQuery :: Foreign

type Elements =
  { forgotten :: JQuery (One "pre")
  , expectedValue :: JQuery (One "span")
  , forgottenValue :: JQuery (One "span")
  , expectedVoi :: JQuery (One "span")
  , error :: JQuery (One "div")
  }

type Inputs = { voiText :: Event String, initialVoiText :: String }

collectElements :: Effect Elements
collectElements = do
  forgotten <- sel "#forgotten"
  expectedValue <- sel "#expected-value"
  forgottenValue <- sel "#forgotten-expected-value"
  expectedVoi <- sel "#voi-result"
  error <- sel "#voi-error"
  pure { forgotten, expectedValue, forgottenValue, expectedVoi, error }

sel ::
     forall tag.
     Selector
  -> Effect (JQuery (One tag))
sel s = J.unsafeSelectOneBecause (s <> " required") s

setup :: Effect Inputs
setup = do
  voiTextEl <- sel "#voi-text"
  voiText <- textAreaEvent voiTextEl
  initialVoiText <- J.getText voiTextEl
  pure { voiText, initialVoiText }

sink ::
     Elements
  -> Either String Unit
  -> InvestigationAndDecisionTree Prob String String (Result String Number)
  -> Effect Unit
sink el error tree = do
  void $ errorTextEl el.error error
  void $ J.setText (show' <<< untagged <<< voi $ tree) el.expectedVoi
  void $ J.setText (show' <<< untagged <<< simpleDecisionTreeValue $ forgotten) el.forgottenValue
  void $ J.setText (show' <<< untagged <<< investigationAndDecisionTreeValue $ tree) el.expectedValue
  void $ J.setText (printSimpleDecisionTree forgotten) el.forgotten
  where
    show' = toStringWith (fixed 2)
    forgotten = VOI.forget tree

main :: Effect Unit
main =
  J.ready do
    inputs <- setup
    Console.log <<< show $ parse inputs.initialVoiText
    let initialTree = unsafeFromRightBecause "Starting text known good" $ parse inputs.initialVoiText
    elements <- collectElements
    sink elements (Right unit) initialTree
    FRP.subscribe (latestSuccess parse initialTree inputs.voiText) (uncurry $ sink elements)

printSimpleDecisionTree ::
     SimpleDecisionTree Prob String (Result String Number) -> String
printSimpleDecisionTree =
  printToYaml <<<
  encodeJson <<< map (encodeChoiceJson (encodeJson <<< un)) <<<
  Array.fromFoldable <<< map wrapChoice <<< SDT.elementary
  where
    un (MkProb n) = n

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
  unwrapInvestigation <=< decodeInvestigationJson (map MkProb <<< decodeJson) <=< parseFromYaml

errorTextEl ::
     forall a tag.
     JQuery (One tag)
  -> Either String a
  -> Effect Unit
errorTextEl el =
  either
    (\t -> J.setText t el *> J.display (unwrap el))
    (\_ -> J.hide $ unwrap el)

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
     forall a tag.
     EventType
  -> (Unit -> Effect a)
  -> JQuery (One tag)
  -> Effect (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\_ _ -> push =<< f unit) el
  pure event

textAreaEvent ::
     JQuery (One "textarea")
  -> Effect (Event String)
textAreaEvent el =
  jqueryEvent (EventType "input") (\_ -> unsafeFromForeign <$> J.getValue el) el
