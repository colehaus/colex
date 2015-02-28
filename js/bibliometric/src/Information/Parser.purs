module Information.Parser where

import Control.Alt
import Control.Alternative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Either
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Set as S
import qualified Data.StrMap as SM
import qualified Data.StrMap.Unsafe as SMU
import qualified Data.Traversable as T
import Data.Tuple
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Information.Types

instance eqParse :: Eq ParseError where
  (==) (ParseError { message = a }) (ParseError { message = b }) = a == b
  (/=) a b = not $ a == b

hRuleP :: forall m. (Monad m) => ParserT String m Unit
hRuleP = do
  string "----"
  many $ string "-"
  return unit

word :: forall m. (Monad m) => ParserT String m String
word = mconcat <$> some (noneOf ["\n", "\r", "\t", " ", ""])

wordSpace :: forall m. (Monad m) => ParserT String m String
wordSpace = do
  w <- word
  many inlineSpace
  return w

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
  return $ Tuple deps var

depsP :: forall m. (Monad m) => ParserT String m [State]
depsP = manyTill (State <$> wordSpace) (lookAhead $ string "|")

stateP :: forall m. (Monad m) => ParserT String m State
stateP = State <$> word

probP :: forall m. (MonadError PmfError m, Monad m) => ParserT String m Prob
probP = do
  w <- word
  lift <<< maybe (throwError $ ProbParse w) prob $ readNumber w

outcomeP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (Tuple State Prob)
outcomeP = do
  s <- stateP
  some inlineSpace
  p <- probP
  return $ Tuple s p

pmfBodyP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m (Tuple State Prob)
pmfBodyP = do
  string "|"
  some inlineSpace
  outcomeP

pmfP :: forall m. (MonadError PmfError m, Monad m) =>
        Variable -> ParserT String m PMF
pmfP v = do
  os <- some (do p <- pmfBodyP
                 whiteSpace
                 return p)
  lift $ pmf v (M.fromList os)

casePmfP :: forall m. (MonadError PmfError m, Monad m) =>
            Variable -> ParserT String m (Tuple [State] PMF)
casePmfP v = do
  d <- depsP
  p <- pmfP v
  return $ Tuple d p

condPmfP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m CondPMF
condPmfP = do
  whiteSpace
  (Tuple vs v) <- headerP
  whiteSpace >> hRuleP >> whiteSpace
  cs <- many1Till (casePmfP v) (hRuleP <|> eof)
  -- depify :: [State] -> Deps
  let depify = deps <<< S.fromList <<< A.zipWith Outcome vs
  lift (T.traverse (firstF depify) cs) >>= 
    lift <<< condPmf <<< M.fromList

networkP :: forall m. (MonadError PmfError m, Monad m) =>
            ParserT String m Network
networkP = do
  whiteSpace
  hRuleP
  cs <- some condPmfP
  lift <<< network $ S.fromList cs

first f (Tuple a c) = Tuple (f a) c
  
firstF :: forall a b c m. (Functor m) => (a -> m b) -> Tuple a c -> m (Tuple b c)
firstF f (Tuple a c) = flip Tuple c <$> f a

-- Assumes all lists are same length
transpose :: forall a. [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = A.map AU.head rows : transpose (A.map AU.tail rows) 

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

-- foreign import undefined :: forall a. a

