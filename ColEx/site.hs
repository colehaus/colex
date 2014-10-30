--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow   ((***))
import           Data.Functor    ((<$>))
import           Data.List       (stripPrefix)
import qualified Data.Map        as M
import           Data.Monoid     (mconcat, (<>))
import           System.FilePath

import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "posts/tag/*.html")
  pages <- buildPaginateWith ((paginateOverflow 1 <$>) . sortChronological)
                            "posts/*"
                            (\n -> (fromCapture (fromGlob "posts/page/*/index.html") $ show n))

  paginateRules pages $ \n pat -> do
    route idRoute
    compile $ do
      let pageCtx = paginateContext pages n
      let ctx = constField "title" "Home" <>
                listField "posts" (tagsCtx <> pageCtx) (loadAll pat) <>
                pageCtx <>
                defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  create ["index.html"] $ do
    route idRoute
    compile $ (load $ paginateLastPage pages :: Compiler (Item String)) >>= makeItem . itemBody

  match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ compile getResourceBody
  create ["combined.css"] $ do
    route idRoute
    compile $ do
      items  <- loadAll "css/*"
      makeItem . compressCss $ concatMap itemBody (items :: [Item String])

  match "js/*" $ compile getResourceBody
  create ["combined.js"] $ do
    route idRoute
    compile $ do
      items <- loadAll "js/*"
      let compressJs = id
      makeItem . compressJs $ concatMap itemBody (items :: [Item String])

  match "posts/*" $ do
      route . customRoute $ \ident -> "posts/" <>
                                     (takeBaseName . toFilePath) ident <>
                                     "/index.html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    tagsCtx
          >>= loadAndApplyTemplate "templates/default.html" tagsCtx
          >>= relativizeUrls
          >>= deIndexUrls

  create ["archive"] $ do
      route $ constRoute "posts/index.html"
      compile $ do
          let archiveCtx =
                  listField "posts" tagsCtx (recentFirst =<< loadAll "posts/*") <>
                  constField "title" "Archive" <>
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls
              >>= deIndexUrls

  match "templates/*" $ compile templateCompiler

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls = return . ((withUrls deIndex) <$>) where
  deIndex u = case stripPrefix (reverse "index.html") (reverse u) of
    Just u' -> reverse u'
    Nothing -> u

tagsCtx :: Context String
tagsCtx = listField "tags" missingField (return []) <> postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

paginateOverflow :: Int -> [a] -> [[a]]
paginateOverflow low xs = if length (last pgs) < low
                          then reverse . uncurry (:) . (mconcat *** id) .
                               (splitAt 2) . reverse $ pgs
                          else pgs where
                            pgs = paginateEvery low xs

paginateLastPage :: Paginate -> Identifier
paginateLastPage pg = paginateMakeId pg $ paginateNumPages pg where
  paginateNumPages :: Paginate -> Int
  paginateNumPages = M.size . paginateMap
