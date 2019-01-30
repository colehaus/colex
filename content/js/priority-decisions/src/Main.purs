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
import FRP.JQuery (textAreaChangeEvent)
import Foreign (Foreign)
import Html as Html
import JQuery (append, create, ready) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (clearOne, setText) as J

foreign import jQuery :: Foreign

main :: Effect Unit
main = do
  J.ready do
    els <- Html.collectElements
    void $
      flip FRP.subscribe (sink DT.maximin els.maximinElements.output) =<<
      textAreaChangeEvent els.maximinElements.input
    void $
      flip FRP.subscribe (sink DT.maximax els.maximaxElements.output) =<<
      textAreaChangeEvent els.maximaxElements.input
    void $
      flip FRP.subscribe (sink DT.leximin els.leximinElements.output) =<<
      textAreaChangeEvent els.leximinElements.input

sink ::
  (DT.Row String -> DT.Row String -> Boolean) ->
  JQuery (One "div") ->
  String ->
  Effect Unit
sink rule output tableText =
  case Table.parse Just Just Just Just Just tableText of
    Left e -> J.setText (show e) output
    Right table -> render output (verdicts rule table)

type PlainTable = Table String String String (NonEmptyList String) (NonEmptyList String)

render :: JQuery (One "div") -> List (Tuple String String) -> Effect Unit
render output results = do
  J.clearOne output
  el <- J.create (listToHtml results)
  J.append el (Newtype.unwrap output)
  where
    listToHtml ds = "<ul>" <> Foldable.foldMap itemToHtml ds <> "</ul>"
      where
        itemToHtml (Tuple l r) = "<li>" <> l <> " " <> " beats " <> r <> "</li>"

verdicts ::
  (DT.Row String -> DT.Row String -> Boolean) ->
  PlainTable ->
  List (Tuple String String)
verdicts pred table =
  List.filter (Tuple.uncurry (/=)) <<<
  List.filter ((==) (Just true) <<< Tuple.uncurry (rows pred table)) $
  List.fromFoldable rowIdPairs
  where
    rowIdPairs = pairs $ Table.rowIds table

pairs :: forall a. Ord a => Set a -> Set (Tuple a a)
pairs xs =
  Set.fromFoldable <<< asList $ Tuple <$> Set.toUnfoldable xs <*> Set.toUnfoldable xs
  where
    asList :: forall b. List b -> List b
    asList = identity

rows ::
  forall column row cell columnId rowId a.
  Eq rowId =>
  (row -> row -> a) -> Table rowId columnId cell row column -> rowId -> rowId -> Maybe a
rows f table rowId1 rowId2 =
  case Tuple (Table.row table rowId1) (Table.row table rowId2) of
    Tuple (Just row1) (Just row2) -> Just $ f row1 row2
    Tuple _ _ -> Nothing
