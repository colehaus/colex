{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow       (first)
import           Control.Monad       ((<=<))
import           Data.Functor        ((<$>))
import qualified Data.Map            as M
import           Data.Monoid         (mconcat, mempty, (<>))
import           System.FilePath     (takeBaseName, takeDirectory, takeFileName)

import           Hakyll
import           Text.Pandoc.Options
import           Text.Regex

main :: IO ()
main = hakyll $ do
  tags <- buildTags postPat (fromCapture "posts/tag/*/index.html")
  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ let
      ctx =
        constField "title" ("Tagged " <> tag) <>
        boolField (tag <> "-page") (const True) <>
        listField "posts"
                  (tagsCtx tags <> postCtx)
                  (recentFirst =<< loadAll pat) <>
                  defaultContext in
      makeItem mempty >>=
      loadAndApplyTemplate "templates/tag.html" ctx >>=
      finish ctx
  create ["tags/index.html"] $ do
    route idRoute
    compile $ do
      let ctx =
            tagCloudField "tag-cloud" 50 180 tags <>
            constField "title" "Tags" <>
            boolField "tags-page" (const True) <>
            defaultContext
      makeItem mempty >>=
        loadAndApplyTemplate "templates/tags.html" ctx >>=
        finish ctx

  pages <- buildPaginateWith
           ((paginateOverflow perPage <$>) . sortChronological)
           postPat
           (fromCapture "posts/page/*/index.html" . show)
  paginateRules pages $ \n pat -> do
    route idRoute
    compile $ let
      pageCtx = paginateContext pages n
      ctx =
        constField "title" ("Page " <> show n) <>
        listField "posts"
                  (teaserField "teaser" "teaser" <>
                   tagsCtx tags <>
                   pageCtx <>
                   postCtx)
                  (recentFirst =<< loadAll pat) <>
        pageCtx <>
        defaultContext in
      makeItem mempty >>=
      loadAndApplyTemplate "templates/index-content.html" ctx >>=
      saveSnapshot "page" >>=
      loadAndApplyTemplate "templates/index.html" ctx >>=
      finish ctx
  create ["index.html"] $ do
    route idRoute
    compile $ let
      ctx =
        boolField "home-page" (const True) <>
        constField "title" "Home" <>
        defaultContext in
      loadSnapshotBody (lastPage pages) "page" >>= makeItem >>=
      loadAndApplyTemplate "templates/index.html" ctx >>=
      finish ctx

  buildPosts tags "posts"
  buildPosts tags "drafts"

  create ["archive"] $ do
      route $ constRoute "posts/index.html"
      compile $ let
        ctx = constField "title" "Archive" <>
              boolField "archive-page" (const True) <>
              listField "posts"
                        (tagsCtx tags <> postCtx)
                        (recentFirst =<< loadAll postPat) <>
                        defaultContext in
        makeItem mempty >>=
        loadAndApplyTemplate "templates/archive.html" ctx >>=
        finish ctx

  match "misc/biblio.csl" $ compile cslCompiler
  match "misc/biblio.bib" $ compile biblioCompiler
  match "templates/*" $ compile templateCompiler
  match "images/**" $ do
      route idRoute
      compile copyFileCompiler
  match "css/*.scss" $ do
    route $ setExtension "css"
    compile compileScss
  includes <- makePatternDependency "css/default/_*.scss"
  rulesExtraDependencies [includes] $ match "css/default/site.scss" $ do
      route $ constRoute "css/default.css"
      compile compileScss
  match defJsPat $ compile getResourceBody
  match "dist/*.js" $ do
    route . customRoute $ \ident ->
      "js/" <> (takeFileName . toFilePath) ident
    compile copyFileCompiler
  match "dist/*.js.map" $ do
    route . customRoute $ \ident ->
      "js/" <> (takeFileName . toFilePath) ident
    compile copyFileCompiler
  match "js/**.js" $ do
    route idRoute
    compile copyFileCompiler
  match "js/*.es" $ do
    route $ setExtension "js"
    compile $ withItemBody (compressJS <=< compileES6) =<< getResourceBody

buildPosts :: Tags -> String -> Rules ()
buildPosts tags dir = do
  match (fromGlob $ dir <> "/*/overlay.md") $ compile pandocCompiler
  overlays <- makePatternDependency (fromGlob $ dir <> "/*/overlay.md")
  match (fromGlob $ dir <> "/*/warnings.md") $ compile getResourceBody
  warnings <- makePatternDependency (fromGlob $ dir <> "/*/warnings.md")
  rulesExtraDependencies [overlays, warnings] $ match (mkPostPat dir) $ do
      route . customRoute $ \ident ->
        dir <> "/" <>
        (takeBaseName . takeDirectory . toFilePath) ident <>
        "/index.html"
      compile $ let
        ctx = warnCtx <> overCtx <> jsCtx <>
              cssCtx <> tagsCtx tags <> postCtx in do
        bib <- load "misc/biblio.bib"
        csl <- load "misc/biblio.csl"
        getResourceBody >>=
          readPandocBiblio readerOpt csl bib >>=
          saveSnapshot "teaser" . (stripVerbatim . demoteHeaders . demoteHeaders <$>) .
            writePandocWith writerOpt >>=
          loadAndApplyTemplate "templates/post.html" ctx >>=
          finish ctx

compileScss :: Compiler (Item String)
compileScss = do
  i <- getResourceString
  let dir = takeDirectory . toFilePath . itemIdentifier $ i
  fmap compressCss <$>
    withItemBody (unixFilter "scss"
                             ["--sourcemap=none", "--trace", "-I", dir]) i

defJsPat :: Pattern
defJsPat = "js/default/*.js" .||. "js/default/*.es"

mkPostPat :: String -> Pattern
mkPostPat postDir = fromGlob $ postDir <> "/*/main.*"

postPat :: Pattern
postPat = mkPostPat "posts"

finish :: Context String -> Item String -> Compiler (Item String)
finish ctx i =
  loadAndApplyTemplate "templates/default.html" ctx i >>=
  ((replaceAll "index.html" (const mempty) <$>) <$>) . relativizeUrls

compileES6 :: String -> Compiler String
compileES6 = unixFilter "babel" ["--es2015"]

compressJS :: String -> Compiler String
compressJS = unixFilter "uglifyjs" ["-cm"]

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  dateField "num-date" "%F" <>
  defaultContext

paginateOverflow :: Int -> [a] -> [[a]]
paginateOverflow low xs =
  case paginateEvery low xs of
    [] -> []
    pgs ->
      if length (last pgs) < low
      then uncurry (:) . first mconcat . splitAt 2 $ pgs
      else pgs

lastPage :: Paginate -> Identifier
lastPage pg = paginateMakeId pg . M.size . paginateMap $ pg

listFieldWith' :: String -> Context a -> (Identifier -> Compiler [a]) ->
                  Context b
listFieldWith' k ctx f =
  listFieldWith k ctx $ (mapM makeItem =<<) . f . itemIdentifier

overCtx :: Context String
overCtx =
  field "overlay" $ loadBody . fromFilePath . (<> "/overlay.md") .
  takeDirectory . toFilePath . itemIdentifier
warnCtx :: Context String
warnCtx =
  listFieldWith' "warnings"
                (details <> summary)
                ((lines <$>) . loadBody . fromFilePath . (<> "/warnings.md") .
                 takeDirectory . toFilePath) where
    split g = return . g . break (== '|') . itemBody
    details = field "details" $ split (tail . snd)
    summary = field "summary" $ split fst

jsCtx :: Context String
jsCtx = listFieldWith' "jses" fileNameCtx $ getListMeta "js"
cssCtx :: Context String
cssCtx = listFieldWith' "csses" fileNameCtx $ getListMeta "css"
fileNameCtx :: Context String
fileNameCtx = field "filename" $ return . itemBody
getListMeta :: MonadMetadata m => String -> Identifier -> m [String]
getListMeta k ident =
  maybe [] (map trim . splitAll ",") . lookupString k <$> getMetadata ident

tagsCtx :: Tags -> Context String
tagsCtx tags =
  listFieldWith' "tags" tagCtx getTags where
    tagCtx = field "tag-name" (return . itemBody) <>
             field "tag-url" (return . toUrl . toFilePath .
                              tagsMakeId tags . itemBody)


-- Special tag that (in combination with code block) isn't interpolated by pandoc
stripVerbatim :: String -> String
stripVerbatim =
  flip (subRegex (mkRegexWithOpts
            ("<p><verbatim></p>\n<pre><code>" <>
             "(.*)" <> "</code></pre>\n</verbatim>") False True))
  "\\1"

extensions :: Extensions
extensions = enableExtension Ext_compact_definition_lists $
             enableExtension Ext_latex_macros $
             enableExtension Ext_raw_html $
             enableExtension Ext_citations $
             enableExtension Ext_footnotes $
             enableExtension Ext_intraword_underscores $
             enableExtension Ext_markdown_in_html_blocks $
             enableExtension Ext_fenced_divs $
             enableExtension Ext_native_divs $
             disableExtension Ext_escaped_line_breaks
             pandocExtensions

readerOpt :: ReaderOptions
readerOpt =
  defaultHakyllReaderOptions { readerExtensions = extensions }

writerOpt :: WriterOptions
writerOpt =
  defaultHakyllWriterOptions {
    writerHtmlQTags = True,
    -- Hakyll + Pandoc don't insert <script> correctly,
    -- so we add dummy URL and do it manually
    writerHTMLMathMethod = MathJax "",
    -- writerTableOfContents = True
    -- writerStandalone = True
    -- writerTemplate = unlines ["$toc$", "$body$"]
    writerExtensions = extensions
    }

perPage :: Int
perPage = 5
