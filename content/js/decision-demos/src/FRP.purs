module FRP where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import FRP.Event (Event)
import FRP.Event as FRP

-- Fire event when either constituent fires.
-- The applicative instance only fires when both events have fired.
combine :: forall a b. a -> b -> Event a -> Event b -> Event (Tuple a b)
combine a b as bs =
  FRP.fold
    (\evt (Tuple a' b') -> either (flip Tuple b') (Tuple a') evt)
    (Left <$> as <|> Right <$> bs)
    (Tuple a b)
