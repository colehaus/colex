{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (first)
import Data.Functor ((<$>))
import qualified Data.Map as M
import Data.Monoid (mconcat, mempty, (<>))
import System.FilePath (takeBaseName, takeDirectory, takeFileName)
import Text.Pandoc.Options

import Hakyll
       hiding (load, match, loadAndApplyTemplate, loadAll, loadBody)
import qualified Hakyll
import HakyllExtension

main :: IO ()
main =
  hakyll $ do
    templates <- match @"template" "templates/*" $ compile templateCompiler
    let defaultTemplate = narrowEx templates "templates/default.html"
    csl <- matchIdentifier @"csl" "misc/biblio.csl" $ compile cslCompiler
    bib <- matchIdentifier @"bib" "misc/biblio.bib" $ compile biblioCompiler
    tags <- buildTags (mkPostPat "posts") (fromCapture "posts/tag/*/index.html")
    (postPat, _, _, abstractPat) <-
      buildPosts
        defaultTemplate
        (narrowEx templates "templates/post.html")
        bib
        csl
        tags
        "posts"
    _ <-
      buildPosts
        defaultTemplate
        (narrowEx templates "templates/post.html")
        bib
        csl
        tags
        "drafts"
    buildTagPages defaultTemplate (narrowEx templates "templates/tag.html") tags
    pages <-
      buildPaginateWith
        ((paginateOverflow perPage <$>) . sortChronological)
        (unBlessedPattern postPat)
        (fromCapture "posts/page/*/index.html" . show)
    buildPages
      defaultTemplate
      (narrowEx templates "templates/index.html")
      (narrowEx templates "templates/index-content.html")
      abstractPat
      tags
      pages
    buildTagIndex
      defaultTemplate
      (narrowEx templates "templates/tags.html")
      tags
    buildIndex defaultTemplate (narrowEx templates "templates/index.html") pages
    _ <- buildCss
    buildArchive
      postPat
      defaultTemplate
      (narrowEx templates "templates/archive.html")
      tags
    _ <-
      match "images/**" $ do
        route idRoute
        compile copyFileCompiler
    -- Copy output from webpack, purescript, &c.
    _ <-
      match "js/dist/*" $ do
        fileInDirectoryRoute "js"
        compile copyFileCompiler
    _ <-
      match "js/cooperatives/vendoredOut/*.js" $ do
        fileInDirectoryRoute "js"
        compile copyFileCompiler
    pure ()

buildPages
  :: Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Blessed "abstract" Pattern
  -> Tags
  -> Paginate
  -> Rules ()
buildPages defaultTemplate indexTemplate indexContentTemplate abstractPat tags pages =
  paginateRules pages $ \n pat -> do
    route idRoute
    compile $
      let pageCtx = paginateContext pages n
          ctx =
            constField "title" ("Page " <> show n) <>
            listField
              "posts"
              (teaserField "teaser" "teaser" <> tagsCtx tags <> pageCtx <>
               abstractCtx abstractPat <> postCtx)
              (recentFirst =<< Hakyll.loadAll pat) <>
            pageCtx <>
            defaultContext
      in makeItem mempty >>= loadAndApplyTemplate indexContentTemplate ctx >>=
         saveSnapshot "page" >>=
         loadAndApplyTemplate indexTemplate ctx >>=
         finish defaultTemplate ctx

buildTagPages
  :: Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Rules ()
buildTagPages defaultTemplate tagTemplate tags =
  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $
      let ctx =
            constField "title" ("Tagged " <> tag) <>
            boolField (tag <> "-page") (const True) <>
            listField
              "posts"
              (tagsCtx tags <> postCtx)
              (recentFirst =<< Hakyll.loadAll pat) <>
            defaultContext
      in makeItem mempty >>= loadAndApplyTemplate tagTemplate ctx >>=
         finish defaultTemplate ctx

buildTagIndex
  :: Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Rules ()
buildTagIndex defaultTemplate tagIndexTemplate tags =
  create ["tags/index.html"] $ do
    route idRoute
    compile $ do
      let ctx =
            tagCloudField "tag-cloud" 50 180 tags <> constField "title" "Tags" <>
            boolField "tags-page" (const True) <>
            defaultContext
      makeItem mempty >>= loadAndApplyTemplate tagIndexTemplate ctx >>=
        finish defaultTemplate ctx

buildIndex
  :: Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Paginate
  -> Rules ()
buildIndex defaultTemplate indexTemplate pages =
  create ["index.html"] $ do
    route idRoute
    compile $
      let ctx =
            boolField "home-page" (const True) <> constField "title" "Home" <>
            defaultContext
      in loadSnapshotBody (lastPage pages) "page" >>= makeItem >>=
         loadAndApplyTemplate indexTemplate ctx >>=
         finish defaultTemplate ctx

buildArchive
  :: Blessed "post" Pattern
  -> Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Rules ()
buildArchive postPat defaultTemplate archiveTemplate tags =
  create ["archive"] $ do
    route $ constRoute "posts/index.html"
    compile $
      let ctx =
            constField "title" "Archive" <>
            boolField "archive-page" (const True) <>
            listField
              "posts"
              (tagsCtx tags <> postCtx)
              (recentFirst =<< loadAll postPat) <>
            defaultContext
      in makeItem mempty >>= loadAndApplyTemplate archiveTemplate ctx >>=
         finish defaultTemplate ctx

buildCss :: Rules (Blessed "css" Pattern)
buildCss = do
  includes <- makePatternDependency "css/libs/_*.scss"
  rulesExtraDependencies [includes] $
    match "css/*.scss" $ do
      route $ setExtension "css"
      compile compileScss

fileInDirectoryRoute :: FilePath -> Rules ()
fileInDirectoryRoute dir =
  route . customRoute $ \ident ->
    dir <> "/" <> (takeFileName . toFilePath) ident

buildPosts
  :: Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Blessed "bib" Identifier
  -> Blessed "csl" Identifier
  -> Tags
  -> String
  -> Rules (Blessed "post" Pattern, Blessed "warnings" Pattern, Blessed "overlay" Pattern, Blessed "abstract" Pattern)
buildPosts defaultTemplate postTemplate bibIdent cslIdent tags dir = do
  overlayPat <-
    match (fromGlob $ dir <> "/*/overlay.md") . compile $
    pandocCompilerWith readerOpt writerOpt
  overlays <- makePatternDependency (fromGlob $ dir <> "/*/overlay.md")
  abstractPat <-
    match (fromGlob $ dir <> "/*/abstract.md") . compile $
    pandocCompilerWith readerOpt writerOpt
  abstracts <- makePatternDependency (fromGlob $ dir <> "/*/abstract.md")
  warningsPat <-
    match (fromGlob $ dir <> "/*/warnings.md") $ compile getResourceBody
  warnings <- makePatternDependency (fromGlob $ dir <> "/*/warnings.md")
  postPat <- rulesExtraDependencies [overlays, warnings, abstracts] $
    match (mkPostPat dir) $ do
      route . customRoute $ \ident ->
        dir <> "/" <> (takeBaseName . takeDirectory . toFilePath) ident <>
        "/index.html"
      compile $
        let ctx =
              warnCtx warningsPat <> overCtx overlayPat <>
              abstractCtx abstractPat <>
              jsCtx <>
              cssCtx <>
              tagsCtx tags <>
              postCtx
        in do bib <- load bibIdent
              csl <- load cslIdent
              getResourceBody >>= readPandocBiblio readerOpt csl bib >>=
                saveSnapshot "teaser" .
                (demoteHeaders . demoteHeaders <$>) . writePandocWith writerOpt >>=
                loadAndApplyTemplate postTemplate ctx >>=
                finish defaultTemplate ctx
  pure (postPat, warningsPat, overlayPat, abstractPat)

compileScss :: Compiler (Item String)
compileScss = do
  i <- getResourceString
  let dir = takeDirectory . toFilePath . itemIdentifier $ i
  fmap compressCss <$>
    withItemBody
      (unixFilter "scss" ["--sourcemap=none", "--trace", "-I", dir])
      i

mkPostPat :: String -> Pattern
mkPostPat postDir = fromGlob $ postDir <> "/*/main.*"

finish
  :: Blessed "template" Identifier
  -> Context String
  -> Item String
  -> Compiler (Item String)
finish defaultTemplate ctx i =
  loadAndApplyTemplate defaultTemplate ctx i >>=
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

listFieldWith' :: String
               -> Context a
               -> (Identifier -> Compiler [a])
               -> Context b
listFieldWith' k ctx f =
  listFieldWith k ctx $ (mapM makeItem =<<) . f . itemIdentifier

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <> dateField "num-date" "%F" <> defaultContext

overCtx :: Blessed "overlay" Pattern -> Context String
overCtx overPat =
  field "overlay" $
  loadBody .
  narrowEx overPat .
  fromFilePath .
  (<> "/overlay.md") . takeDirectory . toFilePath . itemIdentifier

abstractCtx :: Blessed "abstract" Pattern -> Context String
abstractCtx abstractPat =
  field "abstract" $
  loadBody .
  narrowEx abstractPat .
  fromFilePath .
  (<> "/abstract.md") . takeDirectory . toFilePath . itemIdentifier

warnCtx :: Blessed "warnings" Pattern -> Context String
warnCtx warnPat =
  listFieldWith'
    "warnings"
    (details <> summary)
    ((lines <$>) .
     loadBody .
     narrowEx warnPat .
     fromFilePath . (<> "/warnings.md") . takeDirectory . toFilePath)
  where
    split g = pure . g . break (== '|') . itemBody
    details = field "details" $ split (tail . snd)
    summary = field "summary" $ split fst

jsCtx :: Context String
jsCtx = listFieldWith' "jses" fileNameCtx $ getListMeta "js"

cssCtx :: Context String
cssCtx = listFieldWith' "csses" fileNameCtx $ getListMeta "css"

fileNameCtx :: Context String
fileNameCtx = field "filename" $ pure . itemBody

getListMeta
  :: MonadMetadata m
  => String -> Identifier -> m [String]
getListMeta k ident =
  maybe [] (map trim . splitAll ",") . lookupString k <$> getMetadata ident

tagsCtx :: Tags -> Context String
tagsCtx tags = listFieldWith' "tags" tagCtx getTags
  where
    tagCtx =
      field "tag-name" (pure . itemBody) <>
      field "tag-url" (pure . toUrl . toFilePath . tagsMakeId tags . itemBody)

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
readerOpt = defaultHakyllReaderOptions {readerExtensions = extensions}

writerOpt :: WriterOptions
writerOpt =
  defaultHakyllWriterOptions
  { writerHtmlQTags = True
    -- TODO Check if this is actually true
    -- Hakyll + Pandoc don't insert <script> correctly,
    -- so we add dummy URL and do it manually
  , writerHTMLMathMethod = MathJax ""
  , writerExtensions = extensions
  }

perPage :: Int
perPage = 5
