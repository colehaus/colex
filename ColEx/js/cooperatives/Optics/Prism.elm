module Optics.Prism ( set, preview, over, Prism, Prism'
                    , (^?), (?~), (?%), (??)
                    , just_, nothing_) where

import Either (..)
    
type Prism s t a b = { inject : b -> t 
                     , matching : s -> Either t a
                     }
type Prism' s a = Prism s s a a

preview : Prism s t a b -> s -> Maybe a
preview pr = either (always Nothing) Just << pr.matching
infixl 8 ^?
(^?) : s -> Prism s t a b -> Maybe a
(^?) = flip preview
                     
set : Prism s t a b -> b -> s -> t
set pr b = either identity (always <| pr.inject b) << pr.matching
infixr 4 ?~
(?~) : Prism s t a b -> b -> s -> t
(?~) = set

over : Prism s t a b -> (a -> b) -> s -> t
over pr f s = either identity (\a -> set pr (f a) s) <| pr.matching s
infixr 4 ?%
(?%) : Prism s t a b -> (a -> b) -> s -> t      
(?%) = over

infixr 9 ??
(??) : Prism s t c d -> Prism c d a b -> Prism s t a b
p ?? q = Prism (p.inject << q.inject)
              (either Left (either (Left << p.inject) Right << q.matching) << p.matching)

just_ : Prism (Maybe a) (Maybe b) a b
just_ = Prism Just (\s -> case s of
                     Just a -> Right a
                     Nothing -> Left  Nothing)

nothing_ : Prism' (Maybe a) ()
nothing_ = Prism (always Nothing) (\s -> case s of
                                    Just a -> Left s
                                    Nothing -> Right ())