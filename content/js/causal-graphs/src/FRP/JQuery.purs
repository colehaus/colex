module FRP.JQuery where

import Prelude

import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event as FRP
import Foreign (unsafeFromForeign)
import JQuery (preventDefault) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (getValue, on) as J
import Web.Event.Event (EventType)

jqueryEvent ::
  forall a tag.
  EventType ->
  (Unit -> Effect a) ->
  JQuery (One tag) ->
  Effect (Event a)
jqueryEvent eventType f el = do
  { event, push } <- FRP.create
  J.on eventType (\evt _ -> push =<< f unit <* J.preventDefault evt) el
  pure event

textAreaChangeEvent :: JQuery (One "textarea") -> Effect (Tuple String (Event String))
textAreaChangeEvent el = ado
  evt <- jqueryEvent (wrap "change") (\_ -> unsafeFromForeign <$> J.getValue el) el
  initial <- unsafeFromForeign <$> J.getValue el
  in Tuple initial evt

inputTextChangeEvent :: JQuery (One "input") -> Effect (Tuple String (Event String))
inputTextChangeEvent el = ado
  evt <- jqueryEvent (wrap "change") (\_ -> unsafeFromForeign <$> J.getValue el) el
  initial <- unsafeFromForeign <$> J.getValue el
  in Tuple initial evt
