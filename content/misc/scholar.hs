{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<*>))
import Control.Concurrent (threadDelay)
import Control.Monad ((<=<), join)
import Data.Functor ((<$>))
import Data.Monoid (mempty, (<>))
import System.Random

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Text.Parsec

type Article = (String, Int) -- Title and cite count
type Journal = ([Article], [Article]) -- Articles with "a replication" in title and all articles

parsePage :: ByteString -> Either ParseError [Article]
parsePage = runParser (many $ (,) <$> title <*> citeNum) () mempty where
  title = try $ manyTill anyChar (try $ string "<h3 class=\"gs_rt\">") >>
                manyTill anyChar (try $ string "</h3>")
  citeNum = try $ manyTill anyChar (try $ string "Cited by ") >>
                  read <$> many digit

buildUrl :: String -> Bool -> Int -> String
buildUrl journal isRep page = "http://scholar.google.com/scholar" <>
                              "?as_publication=" <> journal <>
                              "&start=" <> show page <>
                              rep where
  rep | isRep = "&q=allintitle:+'a+replication'"
      | otherwise = mempty

runJournal :: Manager -> String -> IO Journal
runJournal m journal = (,) <$> pages True 0 [] <*> pages False 0 [] where
  getUrl m = (responseBody <$>) . flip httpLbs m
  second = 1000000 -- second in microseconds
  pages rep n cs = do r <- randomRIO (50, 80)
                      threadDelay $ r * second -- Wait 50-80 seconds to avoid bot problems
                      req <- parseUrl $ buildUrl journal rep n
                      text <- getUrl m $ req { requestHeaders = [("User-Agent", "Mozilla/5.0 Gecko Firefox/33.0")] }
                      case parsePage text of
                        Left _ -> return cs
                        Right [] -> return cs
                        Right cs' -> pages rep (n + 10) (cs' ++ cs)

summarize :: [Journal] -> [(Float, Float)]
summarize = map sumJournal where
  avg xs = fromIntegral (sum xs) / (fromIntegral $ length xs)
  sumJournal (rep, noRep) = (avg $ map snd rep, avg $ map snd noRep)

main :: IO ()
main = withManager defaultManagerSettings $ \m ->
     print =<< runJournal m "trends"
  -- print . summarize =<< mapConcurrently (runJournal m) journals

journals :: [String]
journals = [ "neuron"
           , "journal of political economy"
           , "american economic review"
           , "advances in physics"
           ]
