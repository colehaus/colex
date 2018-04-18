module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.JQuery (JQuery)
import Control.Monad.Eff.JQuery as J
import DOM (DOM)
import Data (Investigation, Result(..), first, unwrapInvestigation, wrapChoice, wrapInvestigation)
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
import Data.StrMap as StrMap
import Data.Tuple (Tuple(Tuple), fst, snd)
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

-- TODO: Move local imports here


asList :: forall a. List a -> List a
asList = id

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, frp :: FRP | e) Unit
main
  -- TODO: Remove dollar sign
 =
  J.ready $ do
    voiTextEl <- J.select "#voi-text"
    voiTextEvent <- textAreaEvent voiTextEl
    initialText <- J.getText voiTextEl
    let voiTree =
          latestSuccess
            parse
            (unsafeFromRightBecause "Starting text known good" $
             parse initialText)
            voiTextEvent
    forgottenEl <- J.select "#forgotten pre"
    perfectedEl <- J.select "#perfected pre"
    forgottenValueEl <- J.select "#forgotten-expected-value"
    expectedValueEl <- J.select "#expected-value"
    perfectedValueEl <- J.select "#perfected-expected-value"
    voiResultEl <- J.select "#voi-result"
    voiPerfectEl <- J.select "#voi-perfect"
    errorEl <- J.select "#voi-error"
    let forgotten = VOI.forget <<< snd <$> voiTree
    let forgottenText =
          printToYaml <<<
          Generic.encodeJson <<<
          Array.fromFoldable <<< map wrapChoice <<< SDT.elementary <$> forgotten
    let perfected = VOI.perfect <<< VOI.forget <<< snd <$> voiTree
    let perfectedText =
          printToYaml <<<
          Generic.encodeJson <<<
          wrapInvestigation <<< IDT.elementary <<< asStrMaps <$> perfected
    _ <- FRP.subscribe (fst <$> voiTree) (errorTextEl errorEl)
    _ <-
      FRP.subscribe
        (voi <<< snd <$> voiTree)
        (flip J.setText voiResultEl <<< show <<< untagged)
    _ <-
      FRP.subscribe
        (voi <$> perfected)
        (flip J.setText voiPerfectEl <<< show <<< untagged)
    _ <-
      FRP.subscribe
        (simpleDecisionTreeValue <$> forgotten)
        (flip J.setText forgottenValueEl <<< show <<< untagged)
    _ <-
      FRP.subscribe
        (investigationAndDecisionTreeValue <<< snd <$> voiTree)
        (flip J.setText expectedValueEl <<< show <<< untagged)
    _ <-
      FRP.subscribe
        (investigationAndDecisionTreeValue <$> perfected)
        (flip J.setText perfectedValueEl <<< show <<< untagged)
    _ <- FRP.subscribe forgottenText (flip J.setText forgottenEl)
    _ <- FRP.subscribe perfectedText (flip J.setText perfectedEl)
    pure unit

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

asStrMaps ::
     InvestigationAndDecisionTree
       Prob
       (NonEmpty Map String (Result String Number))
       String
       (Result String Number)
  -> InvestigationAndDecisionTree
       Prob
       String
       String
       (Result String Number)
asStrMaps =
  unsafeFromRightBecause "x" <<<
  IDT.lift
    (map $
     first $
     printToYaml <<<
     Argonaut.encodeJson <<<
     StrMap.fromFoldable <<<
     asList <<< Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert)


parse ::
     String
  -> Either String (InvestigationAndDecisionTree Prob String String (Result String Number))
parse =
  lmap show <<<
  VOI.fromElementary <<<
  unwrapInvestigation <<< fixType <=< Generic.decodeJson <=< parseFromYaml
  where
    fixType ::
         forall a.
         Investigation String String String (Result String Number) Prob
      -> Investigation String String String (Result String Number) Prob
    fixType = id

errorTextEl :: forall a e. JQuery -> Either String a -> Eff ( dom :: DOM | e) Unit
errorTextEl el = either (flip J.setText el) (\_ -> J.clear el)

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
