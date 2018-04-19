module Control.Monad.Eff.JQuery.Fancy where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery as J
import DOM (DOM)
import DOM.Event.Types (EventType)
import Data.Array as Array
import Data.Foldable (length)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype as Newtype
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

foreign import kind Quantity
foreign import data Empty :: Quantity
foreign import data One :: Symbol -> Quantity
foreign import data Many :: Quantity

newtype JQuery (q :: Quantity) (selector :: Symbol) = MkJQuery J.JQuery

unwrap :: forall q sel. JQuery q sel -> J.JQuery
unwrap (MkJQuery j) = j

selectOne ::
     forall e sel tag.
     IsSymbol sel
  => SProxy sel
  -> Eff (dom :: DOM | e) (Maybe (JQuery (One tag) sel))
selectOne s = do
  els <- J.toArray =<< J.select (reflectSymbol s)
  pure case Tuple (Array.head els) (length els) of
    Tuple (Just el) 1 -> Just <<< MkJQuery $ el
    _ -> Nothing

unsafeSelectOneBecause ::
     forall e sel tag.
     IsSymbol sel
  => String
  -> SProxy sel
  -> Eff (dom :: DOM | e) (JQuery (One tag) sel)
unsafeSelectOneBecause err = map fromJust <<< selectOne
  where
    fromJust (Just x) = x
    fromJust Nothing = unsafeCrashWith err

on :: forall e q sel.
     EventType
  -> (J.JQueryEvent -> JQuery q sel -> Eff (dom :: DOM | e) Unit)
  -> JQuery q sel
  -> Eff (dom :: DOM | e) Unit
on eventType handler =
  J.on (Newtype.unwrap eventType) (\e q -> handler e (MkJQuery q)) <<< unwrap

getValue :: forall e sel tag. JQuery (One tag) sel -> Eff (dom :: DOM | e) Foreign
getValue = J.getValue <<< unwrap

setText :: forall e sel tag. String -> JQuery (One tag) sel -> Eff (dom :: DOM | e) Unit
setText s = J.setText s <<< unwrap

getText :: forall e sel tag. JQuery (One tag) sel -> Eff (dom :: DOM | e) String
getText = J.getText <<< unwrap
