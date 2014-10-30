--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow   ((***))
import           Data.Functor    ((<$>))
import           Data.List       (stripPrefix)
import qualified Data.Map        as M
import           Data.Monoid     (mconcat, mempty, (<>))
import           System.FilePath

import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "posts/tag/*/index.html")
  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ let ctx = constField "title" ("Tagged " <> tag) <>
                        constField (tag <> "-page") mempty <>
                        listField "posts"
                                  (tagsCtx tags <> postCtx)
                                  (recentFirst =<< loadAll pat) <>
                                  defaultContext in
              makeItem mempty >>=
              loadAndApplyTemplate "templates/tag.html" ctx >>=
              loadAndApplyTemplate "templates/default.html" ctx >>=
              relativizeUrls >>=
              deIndexUrls
  create ["tags/index.html"] $ do
    route idRoute
    compile $ do
      tagCloud <- renderTagCloud 50 180 tags
      let ctx = constField "tag-cloud" tagCloud <>
                constField "title" "Tags" <>
                constField "tags-page" mempty <>
                defaultContext
      makeItem mempty >>=
        loadAndApplyTemplate "templates/tags.html" ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        relativizeUrls >>=
        deIndexUrls

  pages <- buildPaginateWith ((paginateOverflow 2 <$>) . sortChronological)
                            "posts/*"
                            (\n -> (fromCapture (fromGlob "posts/page/*/index.html") $ show n))
  paginateRules pages $ \n pat -> do
    route idRoute
    compile $ let pageCtx = paginateContext pages n
                  ctx = constField "title" "Home" <>
                        constField "home-page" mempty <>
                        listField "posts"
                                  (teaserField "teaser" "teaser" <>
                                   tagsCtx tags <>
                                   pageCtx <>
                                   postCtx)
                                  (loadAll pat) <>
                        pageCtx <>
                        defaultContext in
              makeItem mempty >>=
              loadAndApplyTemplate "templates/index.html" ctx >>=
              loadAndApplyTemplate "templates/default.html" ctx >>=
              relativizeUrls >>=
              deIndexUrls
  create ["index.html"] $ do
    route idRoute
    compile $ (load $ paginateLastPage pages :: Compiler (Item String)) >>= makeItem . itemBody

  match "posts/*" $ do
      route . customRoute $ \ident -> "posts/" <>
                                     (takeBaseName . toFilePath) ident <>
                                     "/index.html"
      compile $ getResourceBody >>=
        (renderPandoc <$>) . saveSnapshot "feed" >>=
        saveSnapshot "teaser" >>=
        loadAndApplyTemplate "templates/post.html" (tagsCtx tags <> postCtx) >>=
        loadAndApplyTemplate "templates/default.html" (tagsCtx tags <> postCtx) >>=
        relativizeUrls >>=
        deIndexUrls

  create ["archive"] $ do
      route $ constRoute "posts/index.html"
      compile $ let ctx = constField "title" "Archive" <>
                          constField "archive-page" mempty <>
                          listField "posts"
                                    (tagsCtx tags <> postCtx)
                                    (recentFirst =<< loadAll "posts/*") <>
                          defaultContext in
                makeItem mempty >>=
                loadAndApplyTemplate "templates/archive.html" ctx >>=
                loadAndApplyTemplate "templates/default.html" ctx >>=
                relativizeUrls >>=
                deIndexUrls

  create ["atom"] $ do
    route $ constRoute "posts/feed/atom.xml"
    compile $ let feedCtx = postCtx <> bodyField "description" in
              loadAllSnapshots "posts/*" "feed" >>= (take 10 <$>) . recentFirst >>=
              renderAtom feedConf feedCtx
  create ["rss"] $ do
    route $ constRoute "posts/feed/rss.xml"
    compile $ let feedCtx = postCtx <> bodyField "description" in
              loadAllSnapshots "posts/*" "feed" >>= (take 10 <$>) . recentFirst >>=
              renderRss feedConf feedCtx


  match "templates/*" $ compile templateCompiler
  match "images/*" $ do
      route idRoute
      compile copyFileCompiler
  match "css/*" $ compile getResourceBody
  create ["combined.css"] $ do
    route idRoute
    compile $ loadAll "css/*" >>= makeItem . compressCss . concatMap itemBody
  match "js/*" $ compile getResourceBody
  create ["combined.js"] $ do
    route idRoute
    compile $ loadAll "js/*" >>= makeItem . compressJs . concatMap itemBody

--TODO: Actual js compressor
compressJs :: String -> String
compressJs = id

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls = return . ((withUrls deIndex) <$>) where
  deIndex u = case stripPrefix (reverse "index.html") (reverse u) of
    Just u' -> reverse u'
    Nothing -> u

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

paginateOverflow :: Int -> [a] -> [[a]]
paginateOverflow low xs =
  if length (last pgs) < low
  then reverse . uncurry (:) . (mconcat *** id) . (splitAt 2) . reverse $ pgs
  else pgs where
    pgs = paginateEvery low xs

paginateLastPage :: Paginate -> Identifier
paginateLastPage pg = paginateMakeId pg $ paginateNumPages pg where
  paginateNumPages :: Paginate -> Int
  paginateNumPages = M.size . paginateMap

tagsCtx :: Tags -> Context String
tagsCtx tags = listFieldWith "tags"
                        tagCtx
                        ((mapM makeItem =<<) . getTags . itemIdentifier) where
  tagCtx = (field "tag-name" $ return . itemBody) <>
           (field "tag-url" $ return . toUrl . toFilePath . (tagsMakeId tags) . itemBody)

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
  { feedTitle       = "Collectively Exhaustive"
  , feedDescription = "A weblog"
  , feedAuthorName  = "Cole Haus"
  , feedAuthorEmail = "colehaus@cryptolab.net"
  , feedRoot        = "http://colex.me"
  }
