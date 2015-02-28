module Test.Main where

import Control.Alternative
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Either
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid.All
import qualified Data.Set as S
import qualified Data.StrMap as SM
import qualified Data.Traversable as T
import Data.Tuple
import Test.Unit
import Text.Parsing.Parser
import Text.Parsing.Parser.String

import Information
import Information.Parser
import Information.Types

import Debug.Trace

deps' = fromJust <<< deps

a1 = Outcome (Variable "a") $ State "1"
a2 = Outcome (Variable "a") $ State "2"

a2Pmf = fromJust $ mkPmf "a" ["1", "2"] [0.5, 0.5]
a3Pmf = fromJust $ mkPmf "a" ["1", "2", "3"] [0.2, 0.4, 0.4]

a2CondPmf =
  fromJust <<< condPmf $ M.fromList [Tuple (deps' S.empty) a2Pmf]

b1 = Outcome (Variable "b") $ State "1"
b2 = Outcome (Variable "b") $ State "2"
b3 = Outcome (Variable "b") $ State "3"

b2Pmf = fromJust $ mkPmf "b" ["1", "2"] [0.5, 0.5]

b2CondPmf =
  fromJust <<< condPmf $ M.fromList [Tuple (deps' S.empty) b2Pmf]

c1 = Outcome (Variable "c") $ State "1"
c2 = Outcome (Variable "c") $ State "2"
     
c2Pmf = fromJust $ mkPmf "c" ["t", "f"] [0.5, 0.5]

c2CondPmf = 
  fromJust $ condPmf $ M.fromList
    [ Tuple (deps' $ S.fromList [a1, b1]) c2Pmf
    , Tuple (deps' $ S.fromList [a1, b2]) c2Pmf
    , Tuple (deps' $ S.fromList [a2, b1]) c2Pmf
    , Tuple (deps' $ S.fromList [a2, b2]) c2Pmf
    ]

half = fromJust $ prob 0.5

inputNetwork = """
----
a b | c P
----
1 1 | t 0.5
    | f 0.5

1 2 | t 0.5
    | f 0.5

2 1 | t 0.5
    | f 0.5

2 2 | t 0.5
    | f 0.5

----
| a P
----
| 1 0.5
| 2 0.5

----
| b P
----
| 1 0.5
| 2 0.5
"""

inputSpacey = """


--------
a b | c P
----
  1 1 | t 0.5
    | f 0.5

1   2 | t 0.5
| f 0.5

2 1 | t 0.5
    | f 0.5
2 2 | t 0.5
    | f 0.5



----
| a P

-------
| 1   0.5
  |   2 0.5

----
| b P
----
| 1 0.5

| 2 0.5
"""
ab'Net = f """
----
| a P
----
| 1 0.9
| 2 0.1
----
a | b P
----
1 | 1 0.5
  | 2 0.5
2 | 1 0.5
  | 2 0.5
"""
a'bNet = f """
----
| a P
----
| 1 0.5
| 2 0.5
----
a | b P
----
1 | 1 0.8
  | 2 0.2
2 | 1 0.6
  | 2 0.4
"""
abNet = f """
----
| a P
----
| 1 0.9
| 2 0.1
----
a | b P
----
1 | 1 0.8
  | 2 0.2
2 | 1 0.6
  | 2 0.4
"""
a'b'Net = f """
----
| a P
----
| 1 0.5
| 2 0.5
----
a | b P
----
1 | 1 0.5
  | 2 0.5
2 | 1 0.5
  | 2 0.5
"""

f = fromRight <<< fromRight <<< flip runParserT1 networkP
fromRight (Right a) = a

main = do
  runTest do
    test "information" do
      assert "doesn't calc infos" $
        S.fromList
        [ Tuple (Variable "a") <<< Entropy <<< average $ runEntropy <$>
          [ fromJust $ aggregateInfo abNet (S.fromList [Variable "a"]) S.empty
          , fromJust $ aggregateInfo abNet (S.fromList [Variable "a"]) (S.fromList [Variable "b"])
          ]
        , Tuple (Variable "b") <<< Entropy <<< average $ runEntropy <$>
          [ fromJust $ aggregateInfo abNet (S.fromList [Variable "b"]) S.empty
          , fromJust $ aggregateInfo abNet (S.fromList [Variable "b"]) (S.fromList [Variable "a"])
          ]
        ] == info abNet
      assert "doesn't calc aggregate info given a variable" $
        (runEntropy $ networkEntropy a'bNet) -
        (runEntropy $ networkEntropy abNet) ~~
        (runEntropy <<< fromJust $ aggregateInfo
          abNet
          (S.fromList [Variable "a"])
          (S.fromList [Variable "b"]))
      assert "doesn't calc info for one variable" $
        (runEntropy $ networkEntropy a'b'Net) -
        (runEntropy $ networkEntropy ab'Net) ~~
        (runEntropy <<< fromJust $ aggregateInfo
          abNet
          (S.fromList [Variable "a"])
          S.empty)
      assert "doesn't calc info for two variable" $
        (runEntropy $ networkEntropy a'b'Net) -
        (runEntropy $ networkEntropy abNet) ~~
        (runEntropy <<< fromJust $ aggregateInfo
          abNet
          (S.fromList [Variable "a", Variable "b"])
          S.empty)
      assert "doesn't calc entropy for binary pmf" $
        1 ~~ runEntropy (pmfEntropy a2Pmf)
      assert "doesn't calc entropy for quartenary pmf" $
        1.02193 ~~ runEntropy (pmfEntropy (fromJust $ mkPmf "a"
                                           ["1", "2", "3", "4"]
                                           [0.8, 0.1, 0.05, 0.05]))
      assert "doesn't calc entropy for network" $
        3 ~~ (runEntropy <<< networkEntropy <<< fromJust <<< network $
              S.fromList [c2CondPmf, a2CondPmf, b2CondPmf])
    test "parse" do
      assert "doesn't parse ideal" $
        (Right <$> network' (S.fromList [c2CondPmf, a2CondPmf, b2CondPmf])) ==
        runParserT1 inputNetwork networkP
      assert "doesn't parse spacey" $
        (Right <$> network' (S.fromList [c2CondPmf, a2CondPmf, b2CondPmf])) ==
        runParserT1 inputSpacey networkP
    test "network" do
      assert "doesn't construct" $
       isRight <<< network' $ S.fromList [c2CondPmf, a2CondPmf, b2CondPmf]
      assert "constructs with missing dependencies" $
       isRight <<< network' $ S.fromList [a2CondPmf, b2CondPmf]
    test "pmf" do
      assert "doesn't construct" $
        isRight $ mkPmf' "a" ["1", "2"] [0.5, 0.5]
      assert "constructs with wrong prob" $
        Left (ProbTotal $ Variable "a") == mkPmf "a" ["1", "2"] [0.6, 0.5]
    test "condPmf" do
      assert "doesn't construct with no dependencies" $
        isRight <<< condPmf' $ M.fromList [Tuple (deps' S.empty) a2Pmf]
      assert "doesn't construct with two binary dependencies" $
        isRight <<< 
        condPmf' $ M.fromList
        [ Tuple (deps' $ S.fromList [b1, c1]) a2Pmf
        , Tuple (deps' $ S.fromList [b1, c2]) a2Pmf
        , Tuple (deps' $ S.fromList [b2, c1]) a2Pmf
        , Tuple (deps' $ S.fromList [b2, c2]) a2Pmf
        ]
      assert "constructs when depending upon self" $
        Left (SelfDep $ Variable "a") == condPmf
        (M.fromList [ Tuple (deps' $ S.fromList [a1]) a3Pmf
                    , Tuple (deps' $ S.fromList [a2]) a3Pmf
                    ])
      assert "constructs with incomplete dependencies" $
        Left (Exhaust $ Variable "a") == condPmf
          (M.fromList [ Tuple (deps' $ S.fromList [b1, c1]) a2Pmf
                      , Tuple (deps' $ S.fromList [b2, c2]) a2Pmf
                      ])
      assert "constructs with mismatch" $
        Left (CondMismatch $ Variable "a") == condPmf
          (M.fromList [ Tuple (deps' $ S.fromList [b1]) a2Pmf
                      , Tuple (deps' $ S.fromList [b2]) a3Pmf
                      , Tuple (deps' $ S.fromList [b3]) a2Pmf
                      ])

-- Extra type information

mkPmf' :: String -> [String] -> [Number] -> Either PmfError PMF
mkPmf' = mkPmf
condPmf' :: M.Map Deps PMF -> Either PmfError CondPMF
condPmf' = condPmf
network' :: S.Set CondPMF -> Either PmfError Network
network' = network

runParserT1 :: forall a. String -> ParserT String (Either PmfError) a ->
               Either PmfError (Either ParseError a)
runParserT1 = runParserT

unParserT1 :: forall a. ParserT String (Either PmfError) a -> String ->
              Either PmfError { input :: String
                              , result :: Either ParseError a
                              , consumed :: Boolean}
unParserT1 = unParserT
