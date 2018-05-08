module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (Selector)
import Control.Monad.Eff.JQuery (display, hide, ready) as J
import Control.Monad.Eff.JQuery.Fancy (JQuery, One)
import Control.Monad.Eff.JQuery.Fancy (getText, getValue, on, setText, unsafeSelectOneBecause) as J
import DOM (DOM)
import DOM.Event.Types (EventType(..))
import Data.Argonaut.Generic.Aeson as Generic
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.Foreign (unsafeFromForeign)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Newtype (unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Yaml (parseFromYaml, printToYaml)
import FRP (FRP)
import FRP.Event (Event)
import FRP.Event (create, fold, subscribe) as FRP
import Math.Probability.Dist (Dist)
import Math.Probability.Information.Value (Value)
import Math.Probability.Information.Value (evMaxDecide, evMaxValue, forget, investigationAndDecisionTreeValue, simpleDecisionTreeValue, valueOfInformation) as VOI
import Math.Probability.Information.Value.InvestigationAndDecisionTree (InvestigationAndDecisionTree)
import Math.Probability.Information.Value.InvestigationAndDecisionTree (fromElementary) as VOI
import Math.Probability.Information.Value.SimpleDecisionTree (SimpleDecisionTree)
import Math.Probability.Information.Value.SimpleDecisionTree as SDT
import Math.Probability.Information.Value.Utility (unsafeFromRightBecause)
import Math.Probability.Prob.Number (Prob(..))

import Data (Result(MkResult), unwrapInvestigation, wrapChoice)

type Elements =
  { forgotten :: JQuery (One "pre")
  , expectedValue :: JQuery (One "span")
  , forgottenValue :: JQuery (One "span")
  , expectedVoi :: JQuery (One "span")
  , error :: JQuery (One "div")
  }

type Inputs = { voiText :: Event String, initialVoiText :: String }

collectElements :: forall e. Eff (dom :: DOM | e) Elements
collectElements = do
  forgotten <- sel "#forgotten"
  expectedValue <- sel "#expected-value"
  forgottenValue <- sel "#forgotten-expected-value"
  expectedVoi <- sel "#voi-result"
  error <- sel "#voi-error"
  pure { forgotten, expectedValue, forgottenValue, expectedVoi, error }

sel ::
     forall e tag.
     Selector
  -> Eff (dom :: DOM | e) (JQuery (One tag))
sel s = J.unsafeSelectOneBecause (s <> " required") s

setup :: forall e. Eff (dom :: DOM, frp :: FRP | e) Inputs
setup = do
  voiTextEl <- sel "#voi-text"
  voiText <- textAreaEvent voiTextEl
  initialVoiText <- J.getText voiTextEl
  pure { voiText, initialVoiText }

sink ::
     forall e. Elements
  -> Either String Unit
  -> InvestigationAndDecisionTree Prob String String (Result String Number)
  -> Eff (dom :: DOM | e) Unit
sink el error tree = do
  void $ errorTextEl el.error error
  void $ J.setText (show' <<< untagged <<< voi $ tree) el.expectedVoi
  void $ J.setText (show' <<< untagged <<< simpleDecisionTreeValue $ forgotten) el.forgottenValue
  void $ J.setText (show' <<< untagged <<< investigationAndDecisionTreeValue $ tree) el.expectedValue
  void $ J.setText (printSimpleDecisionTree forgotten) el.forgotten
  where
    show' = toStringWith (fixed 2)
    forgotten = VOI.forget tree

main :: forall e. Eff (dom :: DOM, frp :: FRP | e) Unit
main =
  J.ready do
    inputs <- setup
    let initialTree = unsafeFromRightBecause "Starting text known good" $ parse inputs.initialVoiText
    elements <- collectElements
    sink elements (Right unit) initialTree
    FRP.subscribe (latestSuccess parse initialTree inputs.voiText) (uncurry $ sink elements)

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

errorTextEl ::
     forall a e tag.
     JQuery (One tag)
  -> Either String a
  -> Eff (dom :: DOM | e) Unit
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
     forall a e tag.
     EventType
  -> (Unit -> Eff (frp :: FRP, dom :: DOM | e) a)
  -> JQuery (One tag)
  -> Eff (dom :: DOM , frp :: FRP | e) (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\_ _ -> push =<< f unit) el
  pure event

textAreaEvent ::
     forall e.
     JQuery (One "textarea")
  -> Eff (dom :: DOM , frp :: FRP | e) (Event String)
textAreaEvent el =
  jqueryEvent (EventType "input") (\_ -> unsafeFromForeign <$> J.getValue el) el
