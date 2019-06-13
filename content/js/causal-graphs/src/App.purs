module App where

import Prelude

import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event as FRP

type App elements rawInput err input result output =
  { elements :: Effect (Maybe elements)
  , readInput :: elements -> Effect (Tuple rawInput (Event rawInput))
  , parse :: rawInput -> Either err input
  , analyze :: input -> result
  , unparse :: result -> output
  , render :: elements -> Maybe output -> Effect Unit
  , error :: elements -> Maybe err -> Effect Unit
  }

runApp ::
  forall elements rawInput err input result output.
  App elements rawInput err input result output -> Effect Unit
runApp app = app.elements >>= traverse_ run
  where
    run els = do
      Tuple init evt <- app.readInput els
      runOnRawInputs init
      FRP.subscribe evt runOnRawInputs
      where
        runOnRawInputs rawInput = do
          app.error els <<< left $ input
          app.render els <<< map (app.unparse <<< app.analyze) <<< right $ input
          where
            input = app.parse rawInput
    right = either (const Nothing) Just
    left = either Just (const Nothing)
