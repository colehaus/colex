module Ticks where

import Graphics.Input (..)

type Generation = Tick
type Tick = Int
type Rate = Float
 
playControls = flow down [ flow right [ button run.handle Play "Play"
                                      , button run.handle Pause "Pause"
                                      , button run.handle Step "Step"
                                      ]
                         , spacer 1 5
                         , flow right [ button run.handle Reset "Reset"
                                      , button run.handle Reseed "Reseed"
                         ]]

data Run = Step | Play | Pause | Reset | Reseed

run : Input Run
run = input Pause

playing : Run -> Bool
playing s = case s of
              Play -> True
              _ -> False

reset : Signal Run -> Signal Tick -> Signal Tick
reset s t = let f (s, t) (current, base) =
                  case s of
                    Reset -> (0, t)
                    Reseed -> (0, t)
                    _ -> (t - base, base) in
            fst <~ foldp f (0, 0) ((,) <~ s ~ t)
                                     
                   
stepTicks : Signal Run -> Signal Tick
stepTicks = countIf (\s -> case s of
                      Step -> True
                      _ -> False)

playTicks : Signal Rate -> Signal Run -> Signal Tick
playTicks r s = let sig = (,) <~ 30 `fpsWhen` (playing <~ s) ~ r in
                floor <~ foldp (\(t, r) acc -> acc + (t * r)) 0 sig

totalTicks : Signal Rate -> Signal Run -> Signal Tick
totalTicks r s = dropRepeats <| (+) <~ reset s (stepTicks s) ~
                   reset s (playTicks r s) 