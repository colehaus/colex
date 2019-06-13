module FRP where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import FRP.Event (Event)
import FRP.Event as FRP

infixr 3 combine as <+>

combine ::
  forall a b.
  (Tuple a (Event a)) -> (Tuple b (Event b)) -> Tuple (Tuple a b) (Event (Tuple a b))
combine (Tuple aInit as) (Tuple bInit bs) =
  Tuple
    (Tuple aInit bInit)
    (FRP.fold
      (\evt (Tuple aAcc bAcc) ->
        either
          (\a -> Tuple a bAcc)
          (\b -> Tuple aAcc b)
          evt)
      (Left <$> as <|> Right <$> bs)
      (Tuple aInit bInit))
