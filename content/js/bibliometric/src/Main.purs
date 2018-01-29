module Main where

import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Data.Either
import Data.Either.Unsafe
import qualified Data.Foldable as F
import Data.Foreign
import Data.Maybe.Unsafe
import qualified Data.Set as S
import qualified Data.String as ST
import Data.Tuple
import Debug.Trace
import DOM
import Text.Parsing.Parser

import Network (netScores, comboProb)
import Network.Parser (networkP)
import Network.Types
import Math.Probability.Information (Entropy (..))

header :: forall eff. [Variable] -> Eff (dom :: DOM | eff) J.JQuery
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

body :: forall eff. [Entropy] -> Eff (dom :: DOM | eff) J.JQuery
body ps = do
  body <- J.create "<tbody>"
  row <- J.create "<tr>"
  label <- J.create "<td>"
  "Score" `J.appendText` label
  label `J.append` row
  F.for_ ps (\(Entropy e) -> do
                cell <- J.create "<td>"
                ST.take 6 (show e) `J.appendText` cell
                cell `J.append` row)
  row `J.append` body

table :: forall eff.
         [Tuple Variable Entropy] -> Eff (dom :: DOM | eff) J.JQuery
table ss = do
  t <- J.create "<table>"
  let ss' = unzip ss
  h <- header $ fst ss'
  b <- body $ snd ss'
  h `J.append` t
  b `J.append` t

-- Type checking aid
runParserT' :: forall a. String -> ParserT String (Either PmfError) a ->
               Either PmfError (Either ParseError a)
runParserT' = runParserT

process :: forall eff. Eff (dom :: DOM | eff) J.JQuery
process = do
  input <- J.select ".net > div > textarea"
  s <- fromRight <<< readString <$> J.getValue input
  let n = runParserT' s networkP
  output <- J.select ".net > div > output"
  J.clear output
  case n of
    Left e -> show e `J.appendText` output
    Right r -> case r of
      Left e -> show e `J.appendText` output
      Right n' -> table (netScores (\n -> fromJust <<< comboProb n) n') >>= flip J.append output

main = J.ready $ do
  button <- J.select ".net > button"
  -- trace "hello"
  process
  J.on "click" (\_ _ -> process) button
