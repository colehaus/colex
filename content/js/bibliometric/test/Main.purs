module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either
import Data.Foldable as F
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set as Set
import Data.Tuple
import Effect.Console as Console
import Partial.Unsafe (unsafePartialBecause)
import Text.Parsing.Parser (runParserT, ParseError())

import Network
import Network.Parser
import Network.Types
import Math.Probability
import Math.Probability.Information

main = do
  example firstStudy
  example secondStudy
  example twoStudies
  example fourStudies

example x = unsafePartialBecause "Test1" $ Console.log <<< show <<< netScores (\n -> fromJust <<< comboProb n) <<< parse $ x

parse :: String -> Network Variable State (Array Variable) (Array State)
parse s = unsafePartialBecause "Test2" $ fromRight <<< fromRight $ e where
  -- Type-checking aid
  e = runParserT s networkP :: Either PmfError
                               (Either ParseError
                                (Network Variable State (Array Variable) (Array State)))


firstStudy = """
----
| A P
----
| t 0.8
| f 0.2
"""

secondStudy = """
----
| A P
----
| t 0.5
| f 0.5

----
A | B P
----
t | t 0.9
  | f 0.1
f | t 0.5
  | f 0.5
"""

twoStudies = """
----
| A P
----
| t 0.8
| f 0.2

----
A | B P
----
t | t 0.9
  | f 0.1
f | t 0.5
  | f 0.5
"""

fourStudies = """
----
| A P
----
| t 0.8
| f 0.2

----
A | B P
----
t | t 0.9
  | f 0.1
f | t 0.5
  | f 0.5

----
A | C P
----
t | t 0.7
  | f 0.3
f | t 0.6
  | f 0.4

----
B C | D P
----
t t | t 0.99
    | f 0.01
t f | t 0.9
    | f 0.1
f t | t 0.8
    | f 0.2
f f | t 0.55
    | f 0.45
"""
