module Main where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Table (Table)
import Data.Table (row, rowIds) as Table
import Data.Table.Parse (parse) as Table
import Data.Tuple (Tuple(..), uncurry)
import DecisionTheory.Ignorance as DT
import DecisionTheory.Utility as Utility
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
    tableText <- textAreaChangeEvent els.input
    FRP.subscribe tableText (sink els.output)

sink :: JQuery (One "div") -> String -> Effect Unit
sink output tableText =
  case Table.parse Just Just Just tableText of
    Left e -> J.setText (show e) output
    Right table -> render output (dominations table)

type PlainTable = Table String String String

render ::
  JQuery (One "div") ->
  Tuple (HashSet (Tuple String String)) (HashSet (Tuple String String)) ->
  Effect Unit
render output (Tuple weak strong) = do
  J.clearOne output
  el <- J.create (listToHtml "weakly" weak <> listToHtml "strongly" strong)
  J.append el (Newtype.unwrap output)
  where
    listToHtml adj ds =
      "<ul class=\"" <> adj <> "\">" <> Foldable.foldMap itemToHtml ds' <> "</ul>"
      where
        ds' = Array.sort <<< HashSet.toArray $ ds
        itemToHtml (Tuple l r) = "<li>" <> l <> " " <> adj <> " dominates " <> r <> "</li>"

dominations ::
  PlainTable ->
  Tuple (HashSet (Tuple String String)) (HashSet (Tuple String String))
dominations table = Tuple (dominations' DT.dominatesWeakly) (dominations' DT.dominatesStrongly)
  where
    dominations' pred =
      HashSet.filter (uncurry (/=)) <<<
      HashSet.filter ((==) (Just true) <<< map pred <<< rowIdsToRows) $
      rowIdPairs
    rowIdPairs = pairs $ Table.rowIds table
    rowIdsToRows =
      Utility.neMultiSet <<< uncurry DT.zipActions <<<
      bimap (Table.row table) (Table.row table)

pairs :: forall a. Hashable a => HashSet a -> HashSet (Tuple a a)
pairs xs =
  HashSet.fromFoldable $ Tuple <$> HashSet.toArray xs <*> HashSet.toArray xs
