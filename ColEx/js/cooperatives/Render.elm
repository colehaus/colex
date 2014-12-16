module Render where

import List as L
import Maybe as M
import Array as A
import Array (Array)
import Dict as D
import Debug (log)

import Optics.Lens (..)
import Optics.Prism as P
import Optics.Prism (..)
import Optics.Traversal as T
import Optics.Traversal (..)

import Cell (Index, Cell, FirmType)
import Cell as C
import Grid (GridSize, Groups, Automaton, Grid)
import Grid as G

type CellSize = Int

-- Darkened ColEx palette hues
capitalColor : Float -> Color
capitalColor = rgba 116 46 153

laborColor : Float -> Color
laborColor = rgba 57 147 57
laborAt0 = rgb 215 233 215

emptyColor : Float -> Color
emptyColor = rgba 156 69 53

both : (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

toAlpha : Float -> Float
toAlpha x = 1 - (1 / logBase e (e + max x 0)) * 0.8

-- We've been pretending that 0,0 is the bottom left corner.
-- Shift to Elm's centered coordinates.
botLeft : GridSize -> Form -> Form
botLeft s = move (toFloat s / -2, toFloat s / -2)

positionCell : GridSize -> CellSize -> Index -> Form -> Form
positionCell gs cs = move << both (\i -> toFloat i * toFloat cs + toFloat cs / 2) 

color : Cell -> Color
color c = 
  case c of
    C.MkEmpty e -> emptyColor << toAlpha <| e ^@ C.cost
    C.MkFirm f ->
      case f ^@ C.firmType of
        C.MkCapital -> capitalColor << toAlpha <| f ^@ C.accum
        C.MkLabor _ -> laborColor << toAlpha <| f ^@ C.accum

cellSize : GridSize -> CellSize
cellSize gs = floor <| toFloat gs / toFloat G.length

-- Expects first index to be greater than second index
border : CellSize -> Index -> Index -> (Path, Path)
border cs (c, r) (c', r') =
  if | r == r' -> let x = toFloat <| c * cs
                      b x = segment (x, toFloat <| r * cs)
                                    (x, toFloat <| (r + 1) * cs)
                  in (b (x + 0.5), b (x - 0.5))
     | c == c' -> let y = toFloat <| r * cs
                      b y = segment (toFloat <| c * cs, y)
                                    (toFloat <| (c + 1) * cs, y)
                  in (b (y + 0.5), b (y - 0.5))

borderStyle : Color -> LineStyle
borderStyle c =
  let s = solid c
  in { s | width <- 2 }

lineOfBorders : CellSize -> Groups -> Array (Index, Cell) -> Array Form
lineOfBorders cs gs a =
  let border' (i, c) (i', c') fs =
    if c !# C.firm ?@ C.groupId /= c' !# C.firm ?@ C.groupId
    then
      let (p, p') = border cs i i'
          go p c =
            M.map (\f ->
                case f ^@ C.firmType of
                  C.MkCapital ->
                    let a = (\(Just a) -> a) <|
                            D.getOrFail (f ^@ C.groupId) gs !#
                            G.groupType @# G.capitalGroup ?@ G.accum
                    -- Place white under border so coloration is independent of cell color
                    in group [ traced (borderStyle white) p
                             , traced (borderStyle << capitalColor <| toAlpha a) p
                             ]
                  (C.MkLabor _) -> traced (borderStyle laborAt0) p) <|
            c ^? C.firm
         in A.append fs << A.fromList <| L.filterMap identity [go p c, go p' c']
    else fs
  in if a == A.empty
  then A.empty
  else snd <| A.foldl (\a (a', fs) -> (a, border' a a' fs))
                      (A.getOrFail 0 a, A.empty)
                      (A.slice 1 (A.length a) a)

borders : GridSize -> Automaton -> Form
borders gs a =
  let cs = cellSize gs
      go f = f (A.append << lineOfBorders cs (a ^@ G.groups))
               A.empty
               (a ^@ G.grid)
  in group << A.toList <| go G.indexedFoldFromBottom `A.append`
                          go G.indexedFoldFromLeft

cells : GridSize -> Grid Cell -> Form
cells gs g =
  let cs = cellSize gs
      draw i cell = positionCell gs cs i <<
                    filled (color cell) <|
                    square (toFloat cs)
  in group << G.toList <| G.indexedMap draw g
  
render : GridSize -> Automaton -> Element
render gs g = collage gs gs [ botLeft gs <| cells gs (g ^@ G.grid)
                            , botLeft gs <| borders gs g
                            ]