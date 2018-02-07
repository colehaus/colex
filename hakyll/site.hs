{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow       (first)
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
  buildTagPages tags
  pages <- buildPaginateWith ((paginateOverflow perPage <$>) . sortChronological) postPat (fromCapture "posts/page/*/index.html" . show)
  buildPages tags pages
  buildTagIndex tags
  buildIndex pages
  buildArchive tags
  buildCss
  buildPosts tags "posts"
  buildPosts tags "drafts"
  match "misc/biblio.csl" $ compile cslCompiler
  match "misc/biblio.bib" $ compile biblioCompiler
  match "templates/*" $ compile templateCompiler
  match "images/**" $ do
      route idRoute
      compile copyFileCompiler
  -- Copy output from webpack, purescript, &c.
  match "dist/*" $ do
    fileInDirectoryRoute "js"
    compile copyFileCompiler
  match "js/cooperatives/vendoredOut/*.js" $ do
    fileInDirectoryRoute "js"
    compile copyFileCompiler

buildPages :: Tags -> Paginate -> Rules ()
buildPages tags pages =
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

buildTagPages :: Tags -> Rules ()
buildTagPages tags =
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

buildTagIndex :: Tags -> Rules ()
buildTagIndex tags =
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

buildIndex :: Paginate -> Rules ()
buildIndex pages =
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

buildArchive :: Tags -> Rules ()
buildArchive tags =
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

buildCss :: Rules ()
buildCss = do
  includes <- makePatternDependency "css/default/_*.scss"
  rulesExtraDependencies [includes] $ match "css/*.scss" $ do
      route $ setExtension "css"
      compile compileScss

fileInDirectoryRoute :: FilePath -> Rules ()
fileInDirectoryRoute dir = route . customRoute $ \ident ->
  dir <> "/" <> (takeFileName . toFilePath) ident

buildPosts :: Tags -> String -> Rules ()
buildPosts tags dir = do
  match (fromGlob $ dir <> "/*/overlay.md") . compile $ pandocCompilerWith readerOpt writerOpt
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
          saveSnapshot "teaser" . (demoteHeaders . demoteHeaders <$>) .
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

mkPostPat :: String -> Pattern
mkPostPat postDir = fromGlob $ postDir <> "/*/main.*"
postPat :: Pattern
postPat = mkPostPat "posts"

finish :: Context String -> Item String -> Compiler (Item String)
finish ctx i =
  loadAndApplyTemplate "templates/default.html" ctx i >>=
  ((replaceAll "index.html" (const mempty) <$>) <$>) . relativizeUrls

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

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  dateField "num-date" "%F" <>
  defaultContext
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

extensions :: Extensions
extensions =
  extensionsFromList
  [ Ext_all_symbols_escapable
  , Ext_auto_identifiers
  , Ext_backtick_code_blocks
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_bracketed_spans
  , Ext_citations
  , Ext_compact_definition_lists
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_fancy_lists
  , Ext_fenced_code_attributes
  , Ext_fenced_code_blocks
  , Ext_fenced_divs
  , Ext_footnotes
  , Ext_grid_tables
  , Ext_header_attributes
  , Ext_implicit_figures
  , Ext_implicit_header_references
  , Ext_inline_code_attributes
  , Ext_inline_notes
  , Ext_intraword_underscores
  , Ext_latex_macros
  , Ext_line_blocks
  , Ext_link_attributes
  , Ext_markdown_in_html_blocks
  , Ext_multiline_tables
  , Ext_native_divs
  , Ext_native_spans
  , Ext_pandoc_title_block
  , Ext_pipe_tables
  , Ext_raw_attribute
  , Ext_raw_html
  , Ext_raw_tex
  , Ext_shortcut_reference_links
  , Ext_simple_tables
  , Ext_smart
  , Ext_space_in_atx_header
  , Ext_startnum
  , Ext_strikeout
  , Ext_subscript
  , Ext_superscript
  , Ext_table_captions
  , Ext_tex_math_dollars
  , Ext_yaml_metadata_block
  ]

readerOpt :: ReaderOptions
readerOpt =
  defaultHakyllReaderOptions { readerExtensions = extensions }

writerOpt :: WriterOptions
writerOpt =
  defaultHakyllWriterOptions {
    writerHtmlQTags = True,
    -- TODO Check if this is actually true
    -- Hakyll + Pandoc don't insert <script> correctly,
    -- so we add dummy URL and do it manually
    writerHTMLMathMethod = MathJax "",
    writerExtensions = extensions
    }

perPage :: Int
perPage = 5
