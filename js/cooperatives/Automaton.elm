module Automaton where

import Debug (..)
import Maybe (..)
import Graphics.Input (..)
import Color (..)
import List as L
import Text (..)
import Window as W

import Slider (..)
import Pseudorandom (Seed, Random)
import Pseudorandom as R
import Pseudorandom.Infix (..)

import Optics.Lens (..)

import Ticks (..)
import Cell (Cell, Altruism)
import Cell as C
import Grid (Automaton, GridSize)
import Grid as G
import Render as R
import Transition

seed : Seed
seed = 2348912

gridLength : Int
gridLength = 25

gridSize : Signal GridSize
gridSize = W.width

main : Signal Element
main = (\s g gs -> flow down [ legends gs
                             , R.render gs g 
                             , spacer 1 5
                             , flow right [ plainText' "Altruism:" , spacer 10 1, altSlider, spacer 10 1
                                          , plainText' "Generation:", spacer 10 1, plainText' (show s)
                                          ]
                             , spacer 1 5
                             , playControls
                             ])
       <~ ticks ~ grid ~ gridSize

initState = let (auto, seed') = G.init 0 <| seed
            in (auto, seed', seed, 0)

altruism : Input Altruism
altruism = input 0

altSlider : Element
altSlider = slider altruism.handle identity { defaultSlider | max <- 1
                                                            , step <- 0.01 }

grid : Signal (Automaton)
grid = (\(g, _, _, _) -> g) <~ foldp update initState ((,,) <~ ticks ~ run.signal ~ altruism.signal)

update : (Generation, Run, Altruism) -> (Automaton, Seed, Seed, Tick) -> (Automaton, Seed, Seed, Tick)
update s (auto, current, base, n) =
  case s of
    (0, Reset, al) -> let (auto', next) = G.init al <| base
                  in (auto', next, base, 0)
    (0, Reseed, al) -> let (auto', next) = G.init al <| current
                   in (auto', next, current, 0)
    (_, Pause, _) -> (auto, current, base, n)
    (n', Step, al) -> step n n' current base auto al
    (n', Play, al) -> step n n' current base auto al

step : Tick -> Tick -> Seed -> Seed -> Automaton -> Altruism ->
       (Automaton, Seed, Seed, Tick)
step n n' current base auto al =
  if n' /= n
  then let (auto', next) = Transition.autoStep (auto |> G.altruism @~ al) <| current
       in (auto', next, base, n')
  else (auto, current, base, n)

ticks : Signal Tick
ticks = totalTicks (constant 0.002) run.signal

labelSquish = 0.95

labels : GridSize -> Form
labels gs = group <| L.map (\(p, a) -> moveX (toFloat gs * p * labelSquish) <<
                             toForm << plainText' <| show a) stops

stops : [(Float, Float)]
stops = L.indexedMap (\i a -> ((toFloat i) / 3, a)) [0, 1, 10, 100]

legend : GridSize -> (Float -> Color) -> Form
legend gs f =
  let gradient' = linear (toFloat gs / -2, 0) (toFloat gs / 2, 0) <<
                  L.map (over snd_ (f << R.toAlpha)) <| stops
  in gradient gradient' (rect (toFloat gs) 10)

legends : GridSize -> Element
legends gs =
  let wealthLabel = plainText' "Wealth"
      (w, _) = sizeOf wealthLabel
      gs' = gs - w - 30 
      wealthLegend =
        flow down [ plainText' "Capital-managed firms"
                  , collage gs' 35
                    [ moveY 15 <| legend gs' R.capitalColor
                    , moveX (toFloat gs' / -4 * (labelSquish + 1)) <| labels gs'
                    , moveY -15 <| legend gs'  R.laborColor
                    ]
                  , plainText' "Labor-managed firms"
                  ]
      (_, h) = sizeOf wealthLegend in
  flow right [
    container w h middle <| wealthLabel,
    spacer 10 h,
    wealthLegend
  ]

plainText' : String -> Element
plainText' = leftAligned << style textStyle << toText
textStyle : Style
textStyle = { defaultStyle | typeface <- ["Open sans"], color <- rgb 24 18 30 }
