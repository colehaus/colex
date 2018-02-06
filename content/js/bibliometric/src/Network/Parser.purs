module Network.Parser where

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array as A
import Data.Maybe (maybe)
import Data.Number as N
import Data.String (fromCharArray) as S
import Data.String.Unsafe (char) as S
import Data.Tuple (Tuple(..), fst, snd)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, manyTill)
import Text.Parsing.Parser.String (eof, noneOf, string, whiteSpace)
import Prelude

import Network.Types (CondPMF, Network, State(..), Variable(..), condPmf, network, space)
import Network.Types.Internal (throwPmf, PmfError(..))
import Math.Probability (Dist, Prob, dist, distProbs, extract, prob)

hRuleP :: forall m. (Monad m) => ParserT String m Unit
hRuleP = do
  _ <- string "----"
  _ <- A.many $ string "-"
  pure unit

word :: forall m. (Monad m) => ParserT String m String
word = S.fromCharArray <$> A.some (noneOf $ S.char <$> ["\n", "\r", "\t", " "])

wordSpace :: forall m. (Monad m) => ParserT String m String
wordSpace = do
  w <- word
  _ <- A.many inlineSpace
  pure w

inlineSpace :: forall m. (Monad m) => ParserT String m String
inlineSpace = string " " <|> string "\t"

newline :: forall m. (Monad m) => ParserT String m String
newline = string "\r" <|> string "\n"

infixl 1 before as >>
before :: forall m a b. (Monad m) => m a -> m b -> m b
before m g = m >>= \_ -> g

depVarsP :: forall m. (Monad m) => ParserT String m (Array Variable)
depVarsP = A.fromFoldable <$> manyTill (Variable <$> wordSpace) (string "|")

mainVarP :: forall m. (Monad m) => ParserT String m Variable
mainVarP = Variable <$> word

headerP :: forall m. (Monad m) => ParserT String m (Tuple (Array Variable) Variable)
headerP = do
  deps <- depVarsP
  _ <- A.some inlineSpace
  var <- mainVarP
  _ <- A.some inlineSpace
  _ <- string "P"
  pure $ Tuple deps var

depsP :: forall m. (Monad m) => ParserT String m (Array State)
depsP = A.fromFoldable <$> manyTill (State <$> wordSpace) (lookAhead $ string "|")

stateP :: forall m. (Monad m) => ParserT String m State
stateP = State <$> word

probP :: forall m. MonadError PmfError m => Monad m => ParserT String m Prob
probP = do
  w <- word
  lift <<< maybe (throwPmf $ "Couldn't parse as probability: " <> w) pure $
    prob =<< N.fromString w

outcomeP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Tuple State Prob)
outcomeP = do
  s <- stateP
  _ <- A.some inlineSpace
  p <- probP
  pure $ Tuple s p

distEleP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Tuple State Prob)
distEleP = do
  _ <- string "|"
  _ <- A.some inlineSpace
  outcomeP

distP :: forall m. MonadError PmfError m => Monad m =>
        ParserT String m (Dist State)
distP = do
  os <- A.some (do p <- distEleP
                   _ <- whiteSpace
                   pure p)
  lift <<< maybe (throwPmf $ "Couldn't construct distribution: " <> show os)
    pure $ dist os

casePmfP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Tuple (Array State) (Dist State))
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
  cs <- A.fromFoldable <$> many1Till casePmfP (hRuleP <|> eof)
  lift $ do
    mainSpace <- space v <<< extract <<< snd =<< maybe (throwError $ PmfError "Foo") pure (A.head cs)
    depSpace <- space vs $ fst <$> cs
    condPmf mainSpace depSpace (distProbs <<< snd <$> cs)

networkP :: forall m. MonadError PmfError m => Monad m =>
            ParserT String m (Network Variable State (Array Variable) (Array State))
networkP = do
  _ <- whiteSpace
  _ <- hRuleP
  cs <- A.some condPmfP
  lift $ network cs
