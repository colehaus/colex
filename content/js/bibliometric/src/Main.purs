module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.JQuery as J
import Control.Monad.Except.Trans (runExceptT)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.Foreign (readString)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.String as ST
import Data.Tuple (Tuple, fst, snd)
import DOM (DOM)
import Text.Parsing.Parser (ParseError, ParserT, runParserT)
import Partial.Unsafe (unsafePartial)
import Prelude

import Network (netScores, comboProb)
import Network.Parser (networkP)
import Network.Types (PmfError, Variable(..))
import Math.Probability.Information (Entropy (..))

header :: forall eff. Array Variable -> Eff (dom :: DOM | eff) J.JQuery
header vs = do
  head <- J.create "<thead>"
  row <- J.create "<tr>"
  h <- J.create "<th>"
  "Variable" `J.appendText` h
  h `J.append` row
  F.for_ vs (\(Variable v) -> do
                cell <- J.create "<th>"
                v `J.appendText` cell
                cell `J.append` row)
  row `J.append` head
  pure row

body :: forall eff. Array Entropy -> Eff (dom :: DOM | eff) J.JQuery
body ps = do
  body' <- J.create "<tbody>"
  row <- J.create "<tr>"
  label <- J.create "<td>"
  "Score" `J.appendText` label
  label `J.append` row
  F.for_ ps (\(Entropy e) -> do
                cell <- J.create "<td>"
                ST.take 6 (show e) `J.appendText` cell
                cell `J.append` row)
  row `J.append` body'
  pure body'

table :: forall eff.
         Array (Tuple Variable Entropy) -> Eff (dom :: DOM | eff) J.JQuery
table ss = do
  t <- J.create "<table>"
  let ss' = A.unzip ss
  h <- header $ fst ss'
  b <- body $ snd ss'
  h `J.append` t
  b `J.append` t
  pure t

-- Type checking aid
runParserT' :: forall a. String -> ParserT String (Either PmfError) a ->
               Either PmfError (Either ParseError a)
runParserT' = runParserT

process :: forall eff. Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit
process = do
  input <- J.select ".net > div > textarea"
  s <- either (throw <<< show) pure =<< unwrap <<< runExceptT <<< readString <$> J.getValue input
  let n = runParserT' s networkP
  output <- J.select ".net > div > output"
  J.clear output
  case n of
    Left e -> show e `J.appendText` output
    Right r -> case r of
      Left e -> show e `J.appendText` output
      Right n' -> table (netScores (\n'' -> unsafePartial $ fromJust <<< comboProb n'') n') >>= flip J.append output

main :: Eff (dom :: DOM, exception :: EXCEPTION) Unit
main = J.ready $ do
  button <- J.select ".net > button"
  -- trace "hello"
  process
  J.on "click" (\_ _ -> process) button
