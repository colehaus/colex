module Optics.Lens ( set, view, over, (^@), (@~), (@%), (@@), Lens, Lens'
                   , fst_, snd_) where

type Lens s t a b = { view : s -> a
                    , set : b -> s -> t
                    }
type Lens' s a = Lens s s a a

view : Lens s t a b -> s -> a
view = .view
infixl 8 ^@
(^@) : s -> Lens s t a b -> a
(^@) = flip view

set : Lens s t a b -> b -> s -> t
set  = .set
infixr 4 ~@
(@~) : Lens s t a b -> b -> s -> t
(@~) = set 

over : Lens s t a b -> (a -> b) -> s -> t
over ln f s = set ln (f <| view ln s) s
infixr 4 @%
(@%) : Lens s t a b -> (a -> b) -> s -> t
(@%) = over

infixr 9 @@
(@@) : Lens s t c d -> Lens c d a b -> Lens s t a b
l @@ m = Lens (view m << view l)
              (over l << set m)

fst_ : Lens (a, c) (b, c) a b
fst_ = Lens fst (\b (_, c) -> (b, c))

snd_ : Lens (c, a) (c, b) a b
snd_ = Lens snd (\b (c, _) -> (c, b))