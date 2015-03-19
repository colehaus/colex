module Test.Main where

import Data.Either
import Data.Either.Unsafe
import qualified Data.Foldable as F
import qualified Data.Set as S
import Data.Tuple
import Debug.Trace
import Text.Parsing.Parser (runParserT, ParseError())

import Network
import Network.Parser
import Network.Types
import Math.Probability hiding (choose, oneOf, ProbList(), Prob(), Dist())
-- import Probability.Internal ((<~), (~~), ProbList(), Prob(), Dist())
import Math.Probability.Information

main = do
  print $ gain' ["a", "b", "c", "d"] [] input''
  let g1 = netScores input''
  print g1
  print <<< F.sum $ (to entropyNum <<< snd) <$> S.toList g1

gain' l r = to entropyNum <<< gain (Variable <$> l) (Variable <$> r)

input'' = fromRight $ fromRight input'
input' :: Either PmfError (Either ParseError
                           (Network Variable State [Variable] [State]))
input' = runParserT netString networkP

netString = """
----
b c | d P
----
t t | t 0.9
    | f 0.1
t f | t 0.85
    | f 0.15
f t | t 0.825
    | f 0.175
f f | t 0.8
    | f 0.2

----
a | c P
----
t | t 0.75
  | f 0.25
f | t 0.7
  | f 0.3

----
a | b P
----
t | t 0.65
  | f 0.35
f | t 0.6
  | f 0.4

----
| a P
----
| t 0.55
| f 0.45
"""
