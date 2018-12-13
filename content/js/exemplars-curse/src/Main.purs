module Main where

import Prelude

import Chart as Chart
import Charts.Vega.Primitive as Vega
import Control.Alt ((<|>))
import Data.Argonaut.Core (fromObject, stringify, toObject) as Argo
import Data.Argonaut.Encode (encodeJson) as Argo
import Data.Either.Nested (either3, in1, in2, in3)
import Data.Foldable (maximumBy)
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (round, toNumber)
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number (fromString)
import Data.Number.Format (precision, toStringWith)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, over1, over2, over3, tuple3, (/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Object as Object
import FRP.Event (Event)
import FRP.Event as FRP
import FRP.JQuery (inputChangeEvent)
import Html as Html
import JQuery (ready) as J
import JQuery.Fancy (setText, width) as J
import Partial.Unsafe (unsafeCrashWith)

foreign import jQuery :: Foreign

main :: Effect Unit
main = do
  J.ready do
    elements <- Html.collectElements
    inputs <- inputsEvents elements.inputs
    (\r -> void $ FRP.subscribe inputs (sink elements r)) =<< Ref.new Nothing
    pure unit

type Inputs =
  { stochasticMax :: Number
  , deterministicMax :: Number
  , numContestants :: Int
  }

sink ::
     Html.Elements
  -> Ref (Maybe Vega.View)
  -> Maybe Inputs
  -> Effect Unit
sink els ref = maybe (log "Nothing") (outputActions <=< randomTrials)
  where
    outputActions trials = do
      J.setText (toStringWith (precision 3) $ meanStochastic trials) els.outputs.meanStochastic
      J.setText (show $ probMax trials) els.outputs.probMax
      maybe
        (createViewWithData <<< List.toUnfoldable $ trials)
        (flip changeData (List.toUnfoldable trials)) =<<
        Ref.read ref
    changeData view trials =
      Vega.changeData
        "main"
        (Both {remove: Vega.ByPredicate $ const true} {insert: Chart.extractRecord <<< encodeAsObject <<< mkTrial <$> trials})
        view
    encodeAsObject = unsafeFromJustBecause "Static object" <<< Argo.toObject <<< Argo.encodeJson
    createViewWithData trials = do
      width <- J.width els.charts.combinedChart
      log <<< Argo.stringify <<< Argo.fromObject $ Object.union (Chart.mkSpec width) (Chart.mkData (mkTrial <$> trials))
      Vega.embed
        "#combined-chart"
        (Argo.fromObject $ Object.union (Chart.mkSpec width) (Chart.mkData (mkTrial <$> trials)))
        (Argo.fromObject Chart.opts)
        (flip Ref.write ref <<< Just)
    mkTrial contestants =
      Chart.MkTrial
        { deterministic
        , stochastic
        , stochasticCombined
        , deterministicCombined
        , combined
        }
      where
        deterministic = (unwrap $ maxDeterministic contestants).deterministic
        stochastic = (unwrap $ maxStochastic contestants).stochastic
        combined' = unwrap $ maxCombined contestants
        stochasticCombined = combined'.stochastic
        deterministicCombined = combined'.deterministic
        combined = combined'.stochastic + combined'.deterministic

latestOf3 :: forall a b c. a -> Event a -> b -> Event b -> c -> Event c -> Event (Tuple3 a b c)
latestOf3 a ea b eb c ec =
  FRP.fold
    (\evt acc -> either3 (flip set1 acc) (flip set2 acc) (flip set3 acc) evt)
    (in1 <$> ea <|> in2 <$> eb <|> in3 <$> ec)
    (tuple3 a b c)
  where
    set1 x = over1 (\_ -> x)
    set2 x = over2 (\_ -> x)
    set3 x = over3 (\_ -> x)

-- TODO: Pull these from the HTML so we don't have to worry about keeping these in sync
defaultStochasticMax :: Number
defaultStochasticMax = 1.0

defaultDeterministicMax :: Number
defaultDeterministicMax = 1.0

defaultNumContestants :: Int
defaultNumContestants = 5

inputsEvents :: Html.Inputs -> Effect (Event (Maybe Inputs))
inputsEvents els = do
  mStochasticMax <- (map fromString) <$> inputChangeEvent els.stochasticMax
  mDeterministicMax <- (map fromString) <$> inputChangeEvent els.deterministicMax
  mNumContestants <- (map (map round <<< fromString)) <$> inputChangeEvent els.numContestants
  pure $
   combineMaybes <$>
   latestOf3
     (Just defaultStochasticMax)
     mStochasticMax
     (Just defaultDeterministicMax)
     mDeterministicMax
     (Just defaultNumContestants)
     mNumContestants
  where
    combineMaybes (mStochasticMax /\ mDeterministicMax /\ mNumContestants /\ _) = do
      stochasticMax <- mStochasticMax
      deterministicMax <- mDeterministicMax
      numContestants <- mNumContestants
      pure { stochasticMax, deterministicMax, numContestants }

newtype Contestant = MkContestant { stochastic :: Number, deterministic :: Number }
derive instance eqContestant :: Eq Contestant
derive instance genericContestant :: Generic Contestant _
derive instance newtypeContestant :: Newtype Contestant _
instance showContestant :: Show Contestant where
  show = genericShow

numTrials :: Int
numTrials = 1000

meanStochastic :: List (List Contestant) -> Number
meanStochastic trials =
  mean $ _.stochastic <<< unwrap <<< maxCombined <$> trials
  where
    mean l = Foldable.sum l / Foldable.length l

probMax :: List (List Contestant) -> Number
probMax trials =
  toNumber (countBy (\trial -> maxDeterministic trial == maxCombined trial) trials) /
  toNumber numTrials
  where
    toTuple contestant = Tuple contestant.stochastic contestant.deterministic

randomTrials :: Inputs -> Effect (List (List Contestant))
randomTrials inputs = List.replicateM numTrials (randomContestants inputs)

randomContestant :: Inputs -> Effect Contestant
randomContestant inputs = do
  stochastic <- randomRange 0.0 inputs.stochasticMax
  deterministic <- randomRange 0.0 inputs.deterministicMax
  pure $ MkContestant { stochastic, deterministic }

randomContestants :: Inputs -> Effect (List Contestant)
randomContestants inputs = List.replicateM inputs.numContestants (randomContestant inputs)

maxDeterministic :: List Contestant -> Contestant
maxDeterministic =
  unsafeFromJustBecause "Empty list makes no sense" <<<
  maximumBy (compare `on` (_.deterministic <<< unwrap))

maxStochastic :: List Contestant -> Contestant
maxStochastic =
  unsafeFromJustBecause "Empty list makes no sense" <<<
  maximumBy (compare `on` (_.stochastic <<< unwrap))

maxCombined :: List Contestant -> Contestant
maxCombined =
  unsafeFromJustBecause "Empty list makes no sense" <<<
  maximumBy (compare `on` ((\c -> c.stochastic + c.deterministic) <<< unwrap))

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

countBy :: forall a. (a -> Boolean) -> List a -> Int
countBy p = Foldable.length <<< List.filter p
