module Main where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.HashSet.Multi (MultiSet)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.NonEmpty (NonEmpty)
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
    wireUp els.maximinElements DT.maximin
    wireUp els.maximaxElements DT.maximax
    wireUp els.leximinElements DT.leximin
    wireUp els.strongDominanceElements DT.dominatesStrongly
    wireUp els.weakDominanceElements DT.dominatesWeakly
  where
    wireUp mEls rule =
      Foldable.for_ mEls $ \els ->
        flip FRP.subscribe (sink rule els.output) =<< textAreaChangeEvent els.input

sink ::
  (NonEmpty MultiSet (Tuple String String) -> Boolean) ->
  JQuery (One "div") ->
  String ->
  Effect Unit
sink rule output tableText =
  case Table.parse Just Just Just tableText of
    Left e -> J.setText (show e) output
    Right table -> render output (verdicts rule table)

type PlainTable = Table String String String

render :: JQuery (One "div") -> HashSet (Tuple String String) -> Effect Unit
render output results = do
  J.clearOne output
  el <- J.create (listToHtml results)
  J.append el (Newtype.unwrap output)
  where
    listToHtml ds = "<ul>" <> Foldable.foldMap itemToHtml ds' <> "</ul>"
      where
        ds' = Array.sort <<< HashSet.toArray $ ds
        itemToHtml (Tuple l r) = "<li>" <> l <> " " <> " beats " <> r <> "</li>"

verdicts ::
  (NonEmpty MultiSet (Tuple String String) -> Boolean) ->
  PlainTable -> HashSet (Tuple String String)
verdicts pred table =
  HashSet.filter (uncurry (/=)) <<<
  HashSet.filter ((==) (Just true) <<< map pred <<< rowIdsToRows) $
  rowIdPairs
  where
    rowIdsToRows =
      Utility.neMultiSet <<< uncurry DT.zipActions <<<
      bimap (Table.row table) (Table.row table)
    rowIdPairs = pairs $ Table.rowIds table

pairs :: forall a. Hashable a => HashSet a -> HashSet (Tuple a a)
pairs xs =
  HashSet.fromFoldable $ Tuple <$> HashSet.toArray xs <*> HashSet.toArray xs
