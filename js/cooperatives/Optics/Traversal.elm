module Optics.Traversal where -- ( set, preview, view, over, Traversal, Traversal'
                        -- , (^#), (#~), (#%), (!#) 
                        -- , (##), (#@), (@#), (#?), (?#), (?@), (@?)) where

import Either (..)
import List
import Maybe (..)
import Debug (..)

import Optics.Lens (Lens, Lens')
import Optics.Lens as L
import Optics.Prism (Prism, Prism')
import Optics.Prism as P


type Traversal s t a b = { view : s -> [a]
                         , set : [b] -> s -> t
                         }
type Traversal' s a = Traversal s s a a

view : Traversal s t a b -> s -> [a]
view = .view
infixl 8 ^#
(^#) : s -> Traversal s t a b -> [a]
(^#) = flip view

preview : Traversal s t a b -> s -> Maybe a
preview tr = (\la -> case la of
               [] -> Nothing
               a :: _ -> Just a) << view tr
infixl 8 !#
(!#) : s -> Traversal s t a b -> Maybe a
(!#) = flip preview

set : Traversal s t a b -> [b] -> s -> t
set = .set
infixl 8 #~
(#~) : Traversal s t a b -> [b] -> s -> t
(#~) = set

over : Traversal s t a b -> (a -> b) -> s -> t
over t f s = set t (List.map f <| view t s) s
infixl 8 #%
(#%) : Traversal s t a b -> (a -> b) -> s -> t
(#%) = over

single : a -> [a]
single a = [a]

maybeToList : Maybe a -> [a]
maybeToList = maybe [] single

infixr 9 ##
(##) : Traversal s t c d -> Traversal c d a b -> Traversal s t a b
p ## q = Traversal (concatMap (view q) << view p) 
                   (\bs -> over p (\x -> set q bs x)) -- Extra lambda necessary because of Elm bug

pToT : Prism s t a b -> Traversal s t a b
pToT pr = Traversal (maybeToList << P.preview pr) 
                    (\bs s -> case pr.matching s of
                      Left t -> t
                      Right _ -> P.set pr (head bs) s)

lToT : Lens s t a b -> Traversal s t a b
lToT ln = Traversal (single << L.view ln) 
                    (\[b] -> L.set ln b) 

infixr 9 #@
(#@) : Traversal s t c d -> Lens c d a b -> Traversal s t a b
t #@ l = t ## lToT l
infixr 9 @#
(@#) : Lens s t c d -> Traversal c d a b -> Traversal s t a b
l @# t = lToT l ## t
infixr 9 #?
(#?) : Traversal s t c d -> Prism c d a b -> Traversal s t a b
t #? p = t ## pToT p
infixr 9 ?#
(?#) : Prism s t c d -> Traversal c d a b -> Traversal s t a b
p ?# t = pToT p ## t
infixr 9 ?@
(?@) : Prism s t c d -> Lens c d a b -> Traversal s t a b
p ?@ l = pToT p ## lToT l
infixr 9 @?
(@?) : Lens s t c d -> Prism c d a b -> Traversal s t a b
l @? p = lToT l ## pToT p
infixr 9 @#@
(@#@) : Lens s t c d -> Lens c d a b -> Traversal s t a b
l @#@ m = lToT l ## lToT m
infixr 9 ?#?
(?#?) : Prism s t c d -> Prism c d a b -> Traversal s t a b
p ?#? q = pToT p ## pToT q