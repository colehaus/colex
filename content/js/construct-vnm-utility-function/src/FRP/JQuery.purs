module FRP.JQuery where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (preventDefault) as J
import Control.Monad.Eff.JQuery.Fancy (JQuery, One)
import Control.Monad.Eff.JQuery.Fancy (getValue, on) as J
import DOM (DOM)
import DOM.Event.Types (EventType)
import Data.Foreign (unsafeFromForeign)
import Data.Newtype (wrap)
import FRP (FRP)
import FRP.Event (Event)
import FRP.Event as FRP

jqueryEvent ::
     forall a e tag.
     EventType
  -> (Unit -> Eff (frp :: FRP, dom :: DOM | e) a)
  -> JQuery (One tag)
  -> Eff (dom :: DOM , frp :: FRP | e) (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\evt _ -> push =<< f unit <* J.preventDefault evt) el
  pure event

textAreaEvent ::
     forall e.
     JQuery (One "textarea")
  -> Eff (dom :: DOM , frp :: FRP | e) (Event String)
textAreaEvent el =
  jqueryEvent (wrap "input") (\_ -> unsafeFromForeign <$> J.getValue el) el

