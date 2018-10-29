module FRP.JQuery where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Foreign (unsafeFromForeign)
import FRP.Event (Event)
import FRP.Event as FRP
import JQuery (preventDefault) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (getValue, on) as J
import Web.Event.Event (EventType)

jqueryEvent ::
     forall a tag.
     EventType
  -> (Unit -> Effect a)
  -> JQuery (One tag)
  -> Effect (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\evt _ -> push =<< f unit <* J.preventDefault evt) el
  pure event

inputChangeEvent :: JQuery (One "input") -> Effect (Event String)
inputChangeEvent el = jqueryEvent (wrap "change") (\_ -> unsafeFromForeign <$> J.getValue el) el
