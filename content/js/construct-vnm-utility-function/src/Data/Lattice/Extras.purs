module Data.Lattice.Extras where

import Prelude hiding (bottom,join,top)

import Data.Generic (class Generic, gShow)
import Data.Lattice
  ( class BoundedJoinSemilattice
  , class BoundedMeetSemilattice
  , class JoinSemilattice
  , class MeetSemilattice
  , bottom, join, meet, top
  )
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, over2)

newtype Meet a = MkMeet a
derive instance newtypeMeet :: Newtype (Meet a) _
derive instance genericMeet :: Generic a => Generic (Meet a)
derive newtype instance eqMeet :: Eq a => Eq (Meet a)
derive newtype instance ordMeet :: Ord a => Ord (Meet a)
derive newtype instance boundedMeet :: Bounded a => Bounded (Meet a)
instance showMeet :: Generic a => Show (Meet a) where
  show = gShow
instance semigroupMeet :: MeetSemilattice a => Semigroup (Meet a) where
  append = over2 MkMeet meet
instance monoidMeet :: BoundedMeetSemilattice a => Monoid (Meet a) where
  mempty = MkMeet top

newtype Join a = MkJoin a
derive instance newtypeJoin :: Newtype (Join a) _
derive instance genericJoin :: Generic a => Generic (Join a)
derive newtype instance eqJoin :: Eq a => Eq (Join a)
derive newtype instance ordJoin :: Ord a => Ord (Join a)
derive newtype instance boundedJoin :: Bounded a => Bounded (Join a)
instance showJoin :: Generic a => Show (Join a) where
  show = gShow
instance semigroupJoin :: JoinSemilattice a => Semigroup (Join a) where
  append = over2 MkJoin join
instance monoidJoin :: BoundedJoinSemilattice a => Monoid (Join a) where
  mempty = MkJoin bottom
