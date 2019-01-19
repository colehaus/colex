module Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Set (Set)
import Data.Set as Set
import Data.Table (Table)
import Data.Table as Table
import Data.Table.Parse (parse) as Table
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import DecisionTheory as DT
import Effect (Effect)
import FRP.Event as FRP
import Foreign (Foreign)
import JQuery (append, create, ready) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (clearOne, setText) as J

import FRP.JQuery (textAreaChangeEvent)
import Html as Html

foreign import jQuery :: Foreign

main :: Effect Unit
main = do
  J.ready do
    els <- Html.collectElements
    tableText <- textAreaChangeEvent els.input
    FRP.subscribe tableText (sink els.output)

sink :: JQuery (One "div") -> String -> Effect Unit
sink output tableText =
  case Table.parse identity identity Just identity Just tableText of
    Left e -> J.setText (show e) output
    Right table -> render output (dominations table)

type PlainTable = Table String String String (NonEmptyList String) (NonEmptyList String)

render :: JQuery (One "div") -> Tuple (List (Tuple String String)) (List (Tuple String String)) -> Effect Unit
render output (Tuple weak strong) = do
  J.clearOne output
  el <- J.create (listToHtml "weakly" weak <> listToHtml "strongly" strong)
  J.append el (Newtype.unwrap output)
  where
    listToHtml adj ds = "<ul class=\"" <> adj <> "\">" <> Foldable.foldMap itemToHtml ds <> "</ul>"
      where
        itemToHtml (Tuple l r) = "<li>" <> l <> " " <> adj <> " dominates " <> r <> "</li>"

dominations :: PlainTable -> Tuple (List (Tuple String String)) (List (Tuple String String))
dominations table = Tuple (dominations' DT.dominatesWeakly) (dominations' DT.dominatesStrongly)
  where
    dominations' pred =
      List.filter (Tuple.uncurry (/=)) <<<
      List.filter ((==) (Just true) <<< Tuple.uncurry (pred table)) $
      List.fromFoldable rowIdPairs
    rowIdPairs = pairs $ Table.rowIds table

pairs :: forall a. Ord a => Set a -> Set (Tuple a a)
pairs xs =
  Set.fromFoldable <<< asList $ Tuple <$> Set.toUnfoldable xs <*> Set.toUnfoldable xs
  where
    asList :: forall b. List b -> List b
    asList = identity
