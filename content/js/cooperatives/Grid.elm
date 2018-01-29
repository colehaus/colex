module Grid where

import Array (Array)
import Array as A
import List as L
import Dict (Dict)
import Dict as D
import Maybe as M
import Either (Either (..))  

import Pseudorandom (Random)
import Pseudorandom as R
import Pseudorandom.Infix (..)

import Optics.Lens (..)
import Optics.Lens as L
import Optics.Prism (Prism, Prism')
import Optics.Traversal (Traversal, Traversal')
import Optics.Traversal as T
import Optics.Traversal (..)

import Cell (Cell (..), Empty, Index, FirmType, GroupId, Discount, Altruism)
import Cell as C

type Generation = Int
type Grid a = Array a
type GridSize = Int

length = 25

init : Altruism -> Random (Automaton)
init a = (\gd -> Automaton gd D.empty 1 a) <$>
         R.combineA (A.repeat (length ^ 2) (MkEmpty <$> C.defEmpty))

type Groups = Dict GroupId Group
type Automaton = { grid       : Grid Cell
                 , groups     : Groups
                 , generation : Generation
                 , altruism   : Altruism
                 }

groupAt : GroupId -> Traversal' Groups Group
groupAt k = Traversal (maybeToList << D.get k)
                      (\bs s -> case bs of
                                  [] -> D.remove k s
                                  [b] -> D.insert k b s)

generation : Lens' Automaton Int
generation = Lens .generation (\a s -> {s | generation <- a})

altruism : Lens' Automaton Float
altruism = Lens .altruism (\a s -> {s | altruism <- a})

grid : Lens' Automaton (Grid Cell)
grid = Lens .grid (\a s -> {s | grid <- a})

groups : Lens' Automaton Groups
groups = Lens .groups (\a s -> {s | groups <- a})

type Group = { clean     : Bool
             , groupType : GroupType
             }

data GroupType = MkCapital CapitalGroup | MkLabor

type CapitalGroup = { accum : Float
                    , threshold : Float
                    }

defCapital : Discount -> Group 
defCapital d = Group False << MkCapital << CapitalGroup 0 <|
               C.capitalShare * C.stepProfit / (1 - (1 - C.deathP) * (1 - d))
defLabor : Group
defLabor = Group False MkLabor

capitalGroup : Prism' GroupType CapitalGroup
capitalGroup = Prism MkCapital (\s -> case s of
                                        MkCapital c -> Right c
                                        _ -> Left s)
             
accum : Lens' CapitalGroup Float
accum = Lens .accum (\a s -> {s | accum <- a})

threshold : Lens' CapitalGroup Float
threshold = Lens .threshold (\a s -> {s | threshold <- a})

clean : Lens' Group Bool
clean = Lens .clean (\a s -> {s | clean <- a})

groupType : Lens' Group GroupType
groupType = Lens .groupType (\a s -> {s | groupType <- a })

indexedFilter : (Index -> a -> Bool) -> Grid a -> Grid a
indexedFilter f = A.foldl (\(i, a) acc ->
                            if f i a
                            then A.push a acc
                            else acc) A.empty << indexedMap (,)

type Column a = Array a
foldFromLeft : (Column a -> b -> b) -> b -> Grid a -> b
foldFromLeft f acc g = 
  let gl = A.length g
  in if gl == length
  then f g acc
  else foldFromLeft f (f (A.slice 0 length g) acc)
                      (A.slice length (A.length g) g) 

type Row a = Array a
foldFromBottom : (Row a -> b -> b) -> b -> Grid a -> b
foldFromBottom f acc g =
  let go r acc = 
        if r == length
        then acc
        else
          let row = A.initialize length (\c -> A.getOrFail (c * length + r) g)
          in go (r + 1) <| f row acc
  in go 0 acc

indexedFoldFromLeft : (Column (Index, a) -> b -> b) -> b -> Grid a -> b
indexedFoldFromLeft f acc = foldFromLeft f acc << indexedMap (,)

indexedFoldFromBottom : (Row (Index, a) -> b -> b) -> b -> Grid a -> b
indexedFoldFromBottom f acc = foldFromBottom f acc << indexedMap (,)

indexedFoldl : (Index -> a -> b -> b) -> b -> Array a -> b
indexedFoldl f acc xs = A.foldl (\(i, a) acc -> f i a acc) acc <|
                        indexedMap (,) xs

indexedMap : (Index -> a -> b) -> Grid a -> Grid b
indexedMap f = map2 f indices

filterMap : (a -> Maybe b) -> Array a -> Array b
filterMap f = A.foldl (maybeCons f) A.empty

maybeCons : (a -> Maybe b) -> a -> Array b -> Array b
maybeCons f mx xs =
  case f mx of
    Just x -> A.push x xs
    Nothing -> xs

index : Index -> Traversal' (Grid a) a
index (col, row) = Traversal (maybeToList << A.get (col * length + row))
                             (\[b] -> A.set (col * length + row) b)

reverse : Array a -> Array a
reverse = A.foldr A.push A.empty

map2 : (a -> b -> c) -> Array a -> Array b -> Array c
map2 f xs ys =
  let go xs ys = 
    case (A.get 0 xs, A.get 0 ys) of
      (Just x, Just y) -> f x y `A.push`
                          go (A.slice 1 (A.length xs) xs) (A.slice 1 (A.length ys) ys)
      _ -> A.empty        
  in reverse <| go xs ys

indices : Array Index
indices = A.initialize (length ^ 2) (\i -> (i // length, i `rem` length))

-- von Neumann neighborhood
neighbors : Grid Cell -> Index -> Array Cell
neighbors g (c, r) = let inBounds n = n >= 0 && n < length
                     in filterMap (\(c, r) ->
                                    if inBounds c && inBounds r
                                    then g !# index (c, r)
                                    else Nothing) <|
                        A.fromList [ (c + 1, r)
                                   , (c - 1, r)
                                   , (c, r + 1)
                                   , (c, r - 1)
                                   ]
                        
genGroupId : Generation -> Index -> GroupId
genGroupId n (c, r) = (c * length + r) * n

maybeToList : Maybe a -> [a]
maybeToList = M.maybe [] (\a -> [a])

dirty : C.Firm -> Groups -> Groups
dirty f = D.update (f ^@ C.groupId) (M.map (L.set clean False))
                
toList = A.toList
foldr = A.foldr
fromList = A.fromList
mapR = R.mapA
combine = R.combineA
map = A.map
empty = A.empty
filter = A.filter
foldl = A.foldl