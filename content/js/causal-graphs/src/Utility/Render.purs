module Utility.Render where

import Prelude

import Data.Either (Either, either)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Effect (Effect)
import JQuery (append, create, display, hide) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (clearOne, setText) as J

newtype Element = MkElement String
derive instance newtypeElement :: Newtype Element _

data ListType = Ol | Ul

renderFoldableAsHtmlList ::
  forall f a.
  Foldable f =>
  ListType -> Element -> (a -> String) -> f a -> Element
renderFoldableAsHtmlList typ empty itemToHtml f
  | Foldable.null f = empty
  | otherwise =
    MkElement $ listOpen typ <> Foldable.foldMap (addLi <<< itemToHtml) f <> listClose typ
  where
    listOpen Ul = "<ul>"
    listOpen Ol = "<ol>"
    listClose Ul = "</ul>"
    listClose Ol = "</ol>"
    addLi s = "<li>" <> s <> "</li>"

replaceElIn :: JQuery (One "div") -> Element -> Effect Unit
replaceElIn output el =
  J.clearOne output *>
  (flip J.append (Newtype.unwrap output) <=< J.create <<< Newtype.unwrap $ el)

renderEither ::
  forall r.
  JQuery (One "div") -> (r -> Effect Unit) -> Either String r -> Effect Unit
renderEither errorEl renderSuccess =
  either
    (\e -> J.setText e errorEl *> J.display (Newtype.unwrap errorEl))
    (\r -> J.hide (Newtype.unwrap errorEl) *> renderSuccess r)

