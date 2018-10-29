module Network.Parser where

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Map as Map
import Data.NonEmpty (fromNonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Number as Number
import Data.Set as Set
import Data.String as String
import Data.String.Unsafe as String
import Data.Tuple (Tuple(..), fst, snd)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, manyTill)
import Text.Parsing.Parser.String (eof, noneOf, string, whiteSpace)
import Prelude

import Network.Types (CondPMF, Network, State(..), Variable(..), condPmf, network, space)
import Network.Types.Internal (throwPmf, PmfError(..))
import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Prob.Number (Prob(..))

type Dist' = Dist Prob

hRuleP :: forall m. (Monad m) => ParserT String m Unit
hRuleP = do
  _ <- string "----"
  _ <- Array.many $ string "-"
  pure unit

word :: forall m. (Monad m) => ParserT String m String
word = String.fromCodePointArray <<< map String.codePointFromChar <$> Array.some (noneOf $ String.char <$> ["\n", "\r", "\t", " "])

wordSpace :: forall m. (Monad m) => ParserT String m String
wordSpace = do
  w <- word
  _ <- Array.many inlineSpace
  pure w

inlineSpace :: forall m. (Monad m) => ParserT String m String
inlineSpace = string " " <|> string "\t"

newline :: forall m. (Monad m) => ParserT String m String
newline = string "\r" <|> string "\n"

infixl 1 before as >>
before :: forall m a b. (Monad m) => m a -> m b -> m b
before m g = m >>= \_ -> g

depVarsP :: forall m. (Monad m) => ParserT String m (Array Variable)
depVarsP = Array.fromFoldable <$> manyTill (Variable <$> wordSpace) (string "|")

mainVarP :: forall m. (Monad m) => ParserT String m Variable
mainVarP = Variable <$> word

headerP :: forall m. (Monad m) => ParserT String m (Tuple (Array Variable) Variable)
headerP = do
  deps <- depVarsP
  _ <- Array.some inlineSpace
  var <- mainVarP
  _ <- Array.some inlineSpace
  _ <- string "P"
  pure $ Tuple deps var

depsP :: forall m. (Monad m) => ParserT String m (Array State)
depsP = Array.fromFoldable <$> manyTill (State <$> wordSpace) (lookAhead $ string "|")

stateP :: forall m. (Monad m) => ParserT String m State
stateP = State <$> word

probP :: forall m. MonadError PmfError m => Monad m => ParserT String m Prob
probP = do
  w <- word
  lift <<< maybe (throwPmf $ "Couldn't parse as probability: " <> w) pure $
    prob =<< Number.fromString w
  where
    prob n | 0.0 <= n && n <= 1.0 = Just $ MkProb n
           | otherwise = Nothing

outcomeP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Tuple State Prob)
outcomeP = do
  s <- stateP
  _ <- Array.some inlineSpace
  p <- probP
  pure $ Tuple s p

distEleP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Tuple State Prob)
distEleP = do
  _ <- string "|"
  _ <- Array.some inlineSpace
  outcomeP

distP :: forall m. MonadError PmfError m => Monad m =>
        ParserT String m (Dist' State)
distP = do
  os <- Array.some (do p <- distEleP
                       _ <- whiteSpace
                       pure p)
  lift <<< maybe (throwPmf $ "Couldn't construct distribution: " <> show os)
    pure $ dist os
  where
    dist = map Dist.make <<< nonEmptyMap <<< Map.fromFoldable
    nonEmptyMap = Indexed.nonEmpty (\mp -> (\mn -> Tuple (Tuple mn.key mn.value) (Map.delete mn.key mp)) <$> Map.findMin mp)

casePmfP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Tuple (Array State) (Dist' State))
casePmfP = do
  d <- depsP
  p <- distP
  pure $ Tuple d p

condPmfP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (CondPMF Variable State (Array Variable) (Array State))
condPmfP = do
  _ <- whiteSpace
  (Tuple vs v) <- headerP
  _ <- whiteSpace >> hRuleP >> whiteSpace
  cs <- Array.fromFoldable <$> many1Till casePmfP (hRuleP <|> eof)
  lift $ do
    mainSpace <- space v <<< extract <<< snd =<< maybe (throwError $ PmfError "Foo") pure (Array.head cs)
    depSpace <- space vs $ fst <$> cs
    condPmf mainSpace depSpace (distProbs <<< snd <$> cs)
  where
    extract = Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values
    distProbs = Array.fromFoldable <<< fromNonEmpty List.Cons <<< Dist.probs


networkP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Network Variable State (Array Variable) (Array State))
networkP = do
  _ <- whiteSpace
  _ <- hRuleP
  cs <- Array.some condPmfP
  lift $ network cs
