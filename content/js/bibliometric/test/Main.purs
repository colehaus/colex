module Test.Main where

import qualified Data.Array as A
import Data.Either
import Data.Either.Unsafe
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe.Unsafe
import qualified Data.Set as S
import Data.Tuple
import Debug.Trace
import Text.Parsing.Parser (runParserT, ParseError())

import Network
import Network.Parser
import Network.Types
import Math.Probability
import Math.Probability.Information
import Math.Probability.Internal ((~~))

main = do
  example firstStudy
  example secondStudy
  example twoStudies
  example fourStudies

example = print <<< netScores (\n -> fromJust <<< comboProb n) <<< parse
  
parse :: String -> Network Variable State [Variable] [State]
parse s = fromRight <<< fromRight $ e where
  -- Type-checking aid
  e = runParserT s networkP :: Either PmfError
                               (Either ParseError
                                (Network Variable State [Variable] [State]))


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
