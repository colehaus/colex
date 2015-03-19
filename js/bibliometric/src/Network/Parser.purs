module Network.Parser where

import Control.Alt
import Control.Alternative
import Control.Bind
import Control.Monad.Error.Class
import Control.Monad.Trans
import qualified Data.Array.Unsafe as AU
import qualified Data.Foldable as F
import Data.Maybe
import Data.Tuple
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Network.Types hiding (PmfError())
import Network.Types.Internal (throwPmf, PmfError())
import Math.Probability hiding (oneOf)

hRuleP :: forall m. (Monad m) => ParserT String m Unit
hRuleP = do
  string "----"
  many $ string "-"
  pure unit

word :: forall m. (Monad m) => ParserT String m String
word = F.mconcat <$> some (noneOf ["\n", "\r", "\t", " ", ""])

wordSpace :: forall m. (Monad m) => ParserT String m String
wordSpace = do
  w <- word
  many inlineSpace
  pure w

inlineSpace :: forall m. (Monad m) => ParserT String m String
inlineSpace = string " " <|> string "\t"

newline :: forall m. (Monad m) => ParserT String m String
newline = string "\r" <|> string "\n"

infixl 1 >>
(>>) :: forall m a b. (Monad m) => m a -> m b -> m b
(>>) m g = m >>= \_ -> g

depVarsP :: forall m. (Monad m) => ParserT String m [Variable]
depVarsP = manyTill (Variable <$> wordSpace) (string "|")

mainVarP :: forall m. (Monad m) => ParserT String m Variable
mainVarP = Variable <$> word

headerP :: forall m. (Monad m) => ParserT String m (Tuple [Variable] Variable)
headerP = do
  deps <- depVarsP
  some inlineSpace
  var <- mainVarP
  some inlineSpace
  string "P"
  pure $ Tuple deps var

depsP :: forall m. (Monad m) => ParserT String m [State]
depsP = manyTill (State <$> wordSpace) (lookAhead $ string "|")

stateP :: forall m. (Monad m) => ParserT String m State
stateP = State <$> word

probP :: forall m. (MonadError PmfError m, Monad m) => ParserT String m Prob
probP = do
  w <- word
  lift <<< maybe (throwPmf $ "Couldn't parse as probability: " <> w) pure $
    prob =<< readNumber w

outcomeP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (Tuple State Prob)
outcomeP = do
  s <- stateP
  some inlineSpace
  p <- probP
  pure $ Tuple s p

distEleP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (Tuple State Prob)
distEleP = do
  string "|"
  some inlineSpace
  outcomeP

distP :: forall m. (MonadError PmfError m, Monad m) =>
        ParserT String m (Dist State)
distP = do
  os <- some (do p <- distEleP
                 whiteSpace
                 pure p)
  lift <<< maybe (throwPmf $ "Couldn't construct distribution: " <> show os)
    pure $ dist os

casePmfP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (Tuple [State] (Dist State))
casePmfP = do
  d <- depsP
  p <- distP
  pure $ Tuple d p

condPmfP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (CondPMF Variable State [Variable] [State])
condPmfP = do
  whiteSpace
  (Tuple vs v) <- headerP
  whiteSpace >> hRuleP >> whiteSpace
  cs <- many1Till casePmfP (hRuleP <|> eof)
  lift $ do
    mainSpace <- space v <<< extract <<< snd $ AU.head cs
    depSpace <- space vs $ fst <$> cs
    condPmf mainSpace depSpace (distProbs <<< snd <$> cs)

networkP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (Network Variable State [Variable] [State])
networkP = do
  whiteSpace
  hRuleP
  cs <- some condPmfP
  lift $ network cs

foreign import readNumber """
  function readNumber(x) {
    var n = parseFloat(x);
    if (n === NaN) {
      return Data_Maybe.Nothing.value;
    } else {
      return Data_Maybe.Just.create(n);
    }
  }
""" :: String -> Maybe Number
