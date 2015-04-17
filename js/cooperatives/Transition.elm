module Transition where

import Dict as D
import Maybe as M
import Either (..)
import Array (Array)
import Array as A
import List as L
import Debug (log)

import Pseudorandom (Random)
import Pseudorandom as R
import Pseudorandom.Infix (..)

import Optics.Lens as L
import Optics.Lens (..)
import Optics.Prism as P
import Optics.Prism (..)
import Optics.Traversal (..)
import Optics.Traversal as T

import Cell (GroupId, Empty, Firm, Cell, Index, Altruism, Discount)
import Cell as C
import Grid (Groups, Grid, Generation, Automaton)
import Grid as G

type Probability = Float
abioP : Probability
abioP= 0.001
deathP : Probability
deathP = 0.1
claimP : Probability
claimP = 0.2
maxDiscount : Float
maxDiscount = 0.2

abiogenesis : Generation -> GroupId -> Groups -> Random (Maybe (Firm, Groups))
abiogenesis n gi gs =
  let birth r = 
    if | r < abioP -> (\r' ->
         Just ( C.defCapital n gi
              , gs |> G.groupAt gi #~ [G.defCapital <| r' * maxDiscount])) <$>
         R.float
       | r < abioP * 2 -> (\r' ->
         Just ( C.defLabor (r' * maxDiscount) n gi
              , gs |> G.groupAt gi #~ [G.defLabor])) <$>
         R.float
       | otherwise -> R.constant Nothing
  in birth =<< R.float

death : Random (Maybe Empty)
death =
  let kill r =
        if | r < deathP -> Just <$> C.defEmpty
           | otherwise -> R.constant Nothing
  in kill =<< R.float
                          
accum : Firm -> Groups -> (Firm, Groups)
accum f gs =
 case f ^@ C.firmType of
  C.MkLabor _ ->
    ( f |> C.accum @% (\a -> a + (f ^@ C.laborProfit) + (f ^@ C.capitalProfit))
    , gs)
  C.MkCapital ->
    -- let groupAccum = G.groupAt (f ^@ C.groupId) ## G.groupType @#
    --                  G.capitalGroup ?@ G.accum in
    ( f |> C.accum @% (+) (f ^@ C.laborProfit)
    , gs |> groupAccum (f ^@ C.groupId) #% (+) (f ^@ C.capitalProfit))

fromMaybe : a -> Maybe a -> a
fromMaybe = flip M.maybe identity

groupAccum : GroupId -> Traversal' Groups Float
groupAccum i = G.groupAt i ## G.groupType @# G.capitalGroup ?@ G.accum 

cellStep : Automaton -> Index -> Automaton -> Random Automaton
cellStep init i curr =
  case curr !# G.grid @# G.index i of
    Just (C.MkEmpty e) ->
      let grown mf =
        case mf of
          Just (f, c, egi) ->
            R.constant (subCost egi c curr
                        |> G.grid @# G.index i #~ [C.MkFirm f]
                        |> G.groups @% G.dirty f)
          Nothing ->
            let abio mf =
              case mf of
                Just (f', gs') ->
                  curr |> G.grid @# G.index i #~ [C.MkFirm f']
                       |> G.groups @~ gs'
                Nothing -> curr
            in abio <$> abiogenesis (curr ^@ G.generation) (G.genGroupId (curr ^@ G.generation) i) (curr ^@ G.groups)
      in grown =<< grow init i e
    Just (C.MkFirm f) ->
      let killed me = 
        case me of
          Just e -> curr |> G.grid @# G.index i #~ [C.MkEmpty e]
          Nothing ->
            let (f', gs') = accum f (curr ^@ G.groups)
            in curr |> G.grid @# G.index i #~ [C.MkFirm f']
                    |> G.groups @~ G.dirty f' gs'
      in killed <$> death

subCost : Either GroupId Index -> Cost -> Automaton -> Automaton
subCost e c a =
  case e of
    Left gi -> a |> G.groups @# groupAccum gi #% (\acc -> acc - c)
    Right i -> a |> G.grid @# G.index i ## C.firm ?@ C.accum #% (\acc -> acc - c)

type Cost = Float

type Growth = (Firm, Cost, Either GroupId Index)
grow : Automaton -> Index -> Empty -> Random (Maybe Growth)
grow at i e =
  (\claimants ->
    if claimants == A.empty
    then R.constant Nothing
    else let (_, gi, mi) = maximumBy (\(t, _, _) -> t) claimants
             n = at ^@ G.generation
         in case mi of
              Just i -> (\r ->
                Just ( C.defLabor (r * maxDiscount) n gi
                     , e ^@ C.cost
                     , Right i)) <$> R.float
              Nothing ->
                R.constant <| Just (C.defCapital n gi
                                   , e ^@ C.cost
                                   , Left gi)) =<<
  (R.lift (G.filterMap identity) << G.combine <<
   G.indexedMap (claim (at ^@ G.altruism) (at ^@ G.groups) e) <|
   G.neighbors (at ^@ G.grid) i)

maximumBy : (a -> comparable) -> Array a -> a
maximumBy f = last << sortBy f << A.toList

type Threshold = Float
type ClaimAttempt = (Threshold, GroupId, Maybe Index)
claim : Altruism -> Groups -> Empty -> Index -> Cell -> Random (Maybe ClaimAttempt)
claim al gp e i c = 
  case c ^? C.firm of
    Nothing -> R.constant Nothing
    Just f ->
      (\r -> 
        if r < claimP
        then 
          case f ^@ C.firmType of
            C.MkLabor l ->
              let acc = f ^@ C.accum
              in if e ^@ C.cost < acc && e ^@ C.cost < (l ^@ C.threshold) al
                 then Just (l ^@ C.threshold <| al, f ^@ C.groupId, Just i)
                 else Nothing
            C.MkCapital ->
              let gi = f ^@ C.groupId
                  g = (\(Just g) -> g) <| gp !# G.groupAt gi ##
                      G.groupType @? G.capitalGroup
                  acc = g ^@ G.accum
              in if e ^@ C.cost < acc && e ^@ C.cost < (g ^@ G.threshold)
                 then Just (g ^@ G.threshold, gi, Nothing)
                 else Nothing
        else Nothing) <$> R.float

-- Remove groups which no longer have any active firms
sweep : Groups -> Groups
sweep = D.filter (\_ gp -> not <| gp ^@ G.clean)

clean : Groups -> Groups
clean = D.map (L.set G.clean True)

autoStep : Automaton -> Random (Automaton)
autoStep at = (L.over G.groups (clean << sweep)) << L.over G.generation ((+) 1) <$>
              G.foldl (\i acc -> cellStep at i =<< acc) (R.constant at) G.indices
