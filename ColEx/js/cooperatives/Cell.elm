module Cell where

import Either (..)

import Pseudorandom.Infix (..)
import Pseudorandom (Random)
import Pseudorandom as R

import Optics.Lens (Lens', Lens)
import Optics.Lens as L
import Optics.Prism (Prism', Prism)
import Optics.Prism as P

laborShare : Float
laborShare = 0.7
capitalShare : Float
capitalShare = 0.3
stepProfit : Float
stepProfit = 0.1
laborAdvantage : Float
laborAdvantage = 1.1
deathP : Float
deathP = 0.1
maxCost = 0.3

type Generation = Int
type Altruism = Float
type Discount = Float

type Column = Int
type Row = Int
type Index = (Column, Row)

data Cell = MkFirm Firm | MkEmpty Empty

firm : Prism' Cell Firm
firm = P.Prism MkFirm (\s -> case s of
                               MkFirm f -> Right f
                               _ -> Left s)

emptyCell : Prism' Cell Empty
emptyCell = P.Prism MkEmpty (\s -> case s of
                                 MkEmpty e -> Right e
                                 _ -> Left s)

type Empty = { cost : Float }
cost : Lens' Empty Float
cost = Lens .cost (\a s -> {s | cost <- a})

type GroupId = Int
type Firm = { laborProfit   : Float
            , capitalProfit : Float
            , accum         : Float
            , groupId       : GroupId
            , birthDate     : Int
            , firmType      : FirmType
            }
accum : Lens' Firm Float
accum = Lens .accum (\a s -> {s | accum <- a})
laborProfit : Lens' Firm Float
laborProfit = Lens .laborProfit (\a s -> {s | laborProfit <- a})
capitalProfit : Lens' Firm Float
capitalProfit = Lens .capitalProfit (\a s -> {s | capitalProfit <- a})
groupId : Lens' Firm GroupId
groupId = Lens .groupId (\a s -> {s | groupId <- a})
birthDate : Lens' Firm Int
birthDate = Lens .birthDate (\a s -> {s | birthDate <- a})
firmType : Lens' Firm FirmType
firmType = Lens .firmType (\a s -> {s | firmType <- a})
    
data FirmType = MkLabor Labor | MkCapital

labor : Prism' FirmType Labor
labor = Prism MkLabor (\s -> case s of
                        MkLabor l -> Right l
                        _ -> Left s)

type Labor = { threshold : Float -> Float
             }

threshold : Lens' Labor (Float -> Float)
threshold = Lens .threshold (\a s -> {s | threshold <- a})

defLabor : Discount -> Generation -> GroupId -> Firm
defLabor d n i =
  let stepAd = stepProfit * laborAdvantage - laborShare * stepProfit
      calcThreshold discount altruism = stepAd * altruism / (1 - (1 - discount) * (1 - deathP))
  in Firm (stepProfit * laborShare * laborAdvantage)
          (stepProfit * capitalShare * laborAdvantage)
          0 i n (MkLabor << Labor <| calcThreshold d)

defCapital : Generation -> GroupId -> Firm
defCapital n i = Firm (stepProfit * laborShare)
                      (stepProfit * capitalShare)
                      0 i n MkCapital
defEmpty : Random Empty
defEmpty = (\r -> Empty <| r * maxCost) <$> R.float
