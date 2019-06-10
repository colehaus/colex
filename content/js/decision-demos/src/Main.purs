module Main where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.Foldable as Foldable
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.HashSet.Multi (MultiSet)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype as Newtype
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.Number as Number
import Data.Proportion as Proportion
import Data.Table (Table)
import Data.Table (row, rowIds) as Table
import Data.Table.Parse (parse, Error) as Table
import Data.Tuple (Tuple(..), uncurry)
import DecisionTheory.Ignorance as DT
import DecisionTheory.Utility as Utility
import Effect (Effect)
import FRP (combine) as FRP
import FRP.Event (subscribe) as FRP
import FRP.JQuery (numInputChangeEvent, textAreaChangeEvent)
import Foreign (Foreign, unsafeFromForeign)
import Html (OptimPessimElements, RuleElements)
import Html as Html
import JQuery (append, create, ready) as J
import JQuery.Fancy (JQuery, One)
import JQuery.Fancy (clearOne, getValue, setText) as J
import Partial.Unsafe (unsafePartialBecause)

foreign import jQuery :: Foreign

main :: Effect Unit
main =
  J.ready do
    els <- Html.collectElements
    traverse_
      (wireUp (analyze Just (verdicts DT.maximin) listOfPairsToHtml))
      els.maximinElements
    traverse_
      (wireUp (analyze Just (verdicts DT.maximax) listOfPairsToHtml))
      els.maximaxElements
    traverse_
      (wireUp (analyze Just (verdicts DT.leximin) listOfPairsToHtml))
      els.leximinElements
    traverse_
      (wireUp (analyze Just (verdicts DT.dominatesStrongly) listOfPairsToHtml))
      els.strongDominanceElements
    traverse_
      (wireUp (analyze Just (verdicts DT.dominatesWeakly) listOfPairsToHtml))
      els.weakDominanceElements
    traverse_
      (wireUp
        (analyze
          Number.fromString
          (verdicts $ DT.indifference Proportion.unMk)
          listOfPairsToHtml))
      els.indifferenceElements
    traverse_
      (wireUp (analyze Number.fromString DT.minimaxRegret listToHtml))
      els.minimaxRegretElements
    traverse_ wireUpOptimPessim els.optimismPessimismElements

wireUp ::
  (String -> Either (Table.Error String String) String) ->
  RuleElements ->
  Effect (Effect Unit)
wireUp analyze' els =
  textAreaChangeEvent els.input >>=
  flip FRP.subscribe
    (either (\e -> J.setText (show e) els.output) (render els.output) <<< analyze')

render :: JQuery (One "div") -> String -> Effect Unit
render output text = do
  J.clearOne output
  el <- J.create text
  J.append el (Newtype.unwrap output)

wireUpOptimPessim :: OptimPessimElements -> Effect (Effect Unit)
wireUpOptimPessim els = do
  αInitial <- numToProp <<< unsafeFromForeign <$> J.getValue els.alpha
  αEvent <- map numToProp <$> numInputChangeEvent els.alpha
  tableTextInitial <- unsafeFromForeign <$> J.getValue els.input
  tableTextEvent <- textAreaChangeEvent els.input
  FRP.subscribe
    (FRP.combine αInitial tableTextInitial αEvent tableTextEvent)
    (\(Tuple α tableText) ->
      either (\e -> J.setText (show e) els.output) (render els.output) $
      analyze
        Number.fromString (verdicts $ DT.optimismPessimism identity α) listOfPairsToHtml
        tableText)
  where
    numToProp n = partial $ fromJust <<< Proportion.mk $ n
    partial = unsafePartialBecause "Input element has max and min"

analyze ::
  forall cell analysis.
  (String -> Maybe cell) ->
  (Table String String cell -> analysis) -> (analysis -> String) ->
  String -> Either (Table.Error String String) String
analyze parseCell runDecisionRule stringifyAnalysis tableText  =
  stringifyAnalysis <<< runDecisionRule <$> Table.parse parseCell Just Just tableText

listToHtml :: NonEmpty HashSet String -> String
listToHtml ds = "<ul>" <> Foldable.foldMap itemToHtml ds' <> "</ul>"
  where
    ds' = Array.sort <<< HashSet.toArray <<< fromNonEmpty HashSet.insert $ ds
    itemToHtml l = "<li>" <> l <> "</li>"

type StringTable = Table String String String
type NumberTable = Table String String Number

listOfPairsToHtml :: HashSet (Tuple String String) -> String
listOfPairsToHtml ds = "<ul>" <> Foldable.foldMap itemToHtml ds' <> "</ul>"
  where
    ds' = Array.sort <<< HashSet.toArray $ ds
    itemToHtml (Tuple l r) = "<li>" <> l <> " " <> " beats " <> r <> "</li>"

verdicts ::
  forall cell.
  Hashable cell =>
  (NonEmpty MultiSet (Tuple cell cell) -> Boolean) ->
  Table String String cell -> HashSet (Tuple String String)
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
