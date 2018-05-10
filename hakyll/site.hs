{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (first)
import Control.Monad ((<=<), (>=>), unless)
import Control.Monad.Trans.Class (lift)
import Data.Functor ((<$>))
import Data.Hashable (hash)
import Data.List (foldl1', stripPrefix)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import System.Directory
  ( createDirectoryIfMissing
  , doesPathExist
  , findExecutable
  , listDirectory
  )
import System.Environment (lookupEnv)
import System.FilePath
  ( joinPath
  , replaceExtension
  , splitPath
  , takeBaseName
  , takeDirectory
  , takeFileName
  )
import System.Process (readProcess)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk (query, walk, walkM)

import Hakyll hiding
  ( create
  , load
  , loadAll
  , loadAllSnapshots
  , loadAndApplyTemplate
  , loadBody
  , makePatternDependency
  , match
  , readPandocBiblio
  )
import qualified Hakyll
import Hakyll.Core.Compiler.Internal (compilerUnsafeIO)
import Hakyll.Core.Rules.Internal (Rules(..))
import Hakyll.Fancy
import HakyllPandoc

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration

unsafeRules :: IO a -> Rules a
unsafeRules = Rules . lift

main :: IO ()
main =
  hakyllWith hakyllConfig $ do
    preprocess $
      createDirectoryIfMissing
        True
        (destinationDirectory hakyllConfig <> "/images/latex")
    webHost <-
      preprocess $
      fromMaybe
        ("http://" <> previewHost hakyllConfig <> ":" <>
         show (previewPort hakyllConfig)) <$>
      lookupEnv "WEB_HOST"
    mathRenderMethod <-
      preprocess $ maybe Mock read <$> lookupEnv "MATH_RENDER_METHOD"
    templates <- match @"template" "templates/*" $ compile templateCompiler
    let defaultTemplate = narrowEx templates "templates/default.html"
    csl <- matchIdentifier @"csl" "misc/biblio.csl" $ compile cslCompiler
    bib <- matchIdentifier @"bib" "misc/biblio.bib" $ compile biblioCompiler
    tags <- buildTags (mkPostPat "posts") (fromCapture "tag/*/index.html")
    chunkMap <- unsafeRules buildChunkMap
    (postPat, _, _, abstractPat) <-
      buildPosts
        defaultTemplate
        (narrowEx templates "templates/post.html")
        bib
        csl
        tags
        chunkMap
        "posts"
    _ <-
      buildPosts
        defaultTemplate
        (narrowEx templates "templates/post.html")
        bib
        csl
        tags
        chunkMap
        "drafts"
    feedIdent <-
      buildAtom
        mathRenderMethod
        (destinationDirectory hakyllConfig)
        webHost
        postPat
        bib
        csl
    buildTagPages defaultTemplate (narrowEx templates "templates/tag.html") tags
    paginate <-
      buildPaginateWith
        ((paginateOverflow perPage <$>) . sortChronological)
        (unBlessedPattern postPat)
        (fromCapture "page/*/index.html" . show)
    buildPages
      defaultTemplate
      (narrowEx templates "templates/index.html")
      (narrowEx templates "templates/index-content.html")
      abstractPat
      tags
      paginate
      chunkMap
    tagIndexIdent <-
      buildTagIndex
        defaultTemplate
        (narrowEx templates "templates/tags.html")
        tags
    indexIdent <-
      buildIndex
        defaultTemplate
        (narrowEx templates "templates/index.html")
        paginate
        chunkMap
    scssPat <- match "css/libs/**_*.scss" $ compile copyFileCompiler
    _ <- buildCss scssPat
    archiveIdent <-
      buildArchive
        postPat
        defaultTemplate
        (narrowEx templates "templates/archive.html")
        tags
    _ <-
      match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler
    _ <-
      match "CNAME" $ do
        route idRoute
        compile copyFileCompiler
    _ <-
      match "images/**" $ do
        route idRoute
        compile copyFileCompiler
    _ <-
      match "data/online/**" $ do
        route . customRoute $ \ident ->
          (joinPath . filter ("online/" /=) . splitPath . toFilePath) ident
        compile copyFileCompiler
    -- Copy output from webpack, purescript, &c.
    _ <-
      match ("js/dist/*" .&&. foldMap complement purescriptProjects) $ do
        fileInDirectoryRoute "js"
        compile copyFileCompiler
    _ <-
      match
        ("js/cooperatives/vendoredOut/*.js" .||.
         foldl1' (.||.) purescriptProjects) $ do
        fileInDirectoryRoute "js"
        compile uglifyCompiler
    _ <-
      buildSitemap
        (narrowEx templates "templates/sitemap.xml")
        postPat
        feedIdent
        tagIndexIdent
        indexIdent
        archiveIdent
    pure ()

purescriptProjects :: [Pattern]
purescriptProjects =
  [ "js/dist/bibliometric.js"
  , "js/dist/value-of-information-calculator.js"
  , "js/dist/construct-vnm-utility-function.js"
  ]

buildChunkMap :: IO (Map String [String])
buildChunkMap = chunkMapFromStrings <$> listDirectory "js/dist"

chunkMapFromStrings ::
     (Applicative f, Monoid (f String)) => [String] -> Map String (f String)
chunkMapFromStrings files =
  Map.fromListWith (<>) $
  (\file -> (, pure $ stripJsSuffix file) <$> extractChunks file) =<<
  filter ('~' `elem`) files
  where
    stripJsSuffix = fromMaybe (error "Not '.js'") . stripSuffix ".js"

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf = fmap reverse . stripPrefix (reverse suf) . reverse

extractChunks :: String -> [String]
extractChunks fp =
  case stripSuffix ".js" <=< stripPrefix "vendors~" $ fp of
    Just chunkString -> splitOn "~" chunkString
    Nothing -> error "Not just a vendor chunk"

prodHost :: String
prodHost = "https://www.col-ex.org"

buildSitemap ::
     Blessed "template" Identifier
  -> Blessed "post" Pattern
  -> Blessed "feed" Identifier
  -> Blessed "index" Identifier
  -> Blessed "index" Identifier
  -> Blessed "index" Identifier
  -> Rules (Blessed "map" Identifier)
buildSitemap sitemapTemplate postsPat feedIdent tagIndexIdent indexIdent archiveIdent =
  (head <$>) . create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPat
      feed <- load feedIdent
      tagIndex <- load tagIndexIdent
      index <- load indexIdent
      archive <- load archiveIdent
      tags <- Hakyll.loadAll "tag/*/index.html"
      pages <- Hakyll.loadAll "page/*/index.html"
      makeItem mempty >>=
        loadAndApplyTemplate
          sitemapTemplate
          (mconcat
             [ listField
                 "entries"
                 (postCtx <> constField "host" prodHost)
                 (pure $
                  feed : tagIndex : index : archive : posts <> tags <> pages)
             , defaultContext
             ])

uglifyCompiler :: Compiler (Item String)
uglifyCompiler =
  withItemBody (unixFilter "uglifyjs" ["--compress", "--mangle"]) =<<
  getResourceBody

buildPages ::
     Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Blessed "abstract" Pattern
  -> Tags
  -> Paginate
  -> Map String [String]
  -> Rules ()
buildPages defaultTemplate indexTemplate indexContentTemplate abstractPat tags pages chunkMap =
  paginateRules pages $ \n pat -> do
    route idRoute
    compile $
      let pageCtx = paginateContext pages n
          ctx =
            constField "title" ("Page " <> show n) <>
            listField
              "posts"
              (teaserField "teaser" "teaser" <> tagsCtx tags <> pageCtx <>
               abstractCtx abstractPat <>
               postCtx)
              (recentFirst =<< Hakyll.loadAll pat) <>
            pageCtx <>
            customElementsCtx chunkMap <>
            defaultContext
       in makeItem mempty >>= loadAndApplyTemplate indexContentTemplate ctx >>=
          saveSnapshot "page" >>=
          loadAndApplyTemplate indexTemplate ctx >>=
          finish defaultTemplate ctx

buildTagPages ::
     Blessed "template" Identifier
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

buildTagIndex ::
     Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Rules (Blessed "index" Identifier)
buildTagIndex defaultTemplate tagIndexTemplate tags =
  (head <$>) . create ["tags/index.html"] $ do
    route idRoute
    compile $ do
      let ctx =
            tagCloudField "tag-cloud" 50 180 tags <> constField "title" "Tags" <>
            boolField "tags-page" (const True) <>
            defaultContext
      makeItem mempty >>= loadAndApplyTemplate tagIndexTemplate ctx >>=
        finish defaultTemplate ctx

buildIndex ::
     Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Paginate
  -> Map String [String]
  -> Rules (Blessed "index" Identifier)
buildIndex defaultTemplate indexTemplate pages chunkMap =
  (head <$>) . create ["index.html"] $ do
    route idRoute
    compile $
      let ctx =
            boolField "home-page" (const True) <> constField "title" "Home" <>
            customElementsCtx chunkMap <>
            defaultContext
       in loadSnapshotBody (lastPage pages) "page" >>= makeItem >>=
          loadAndApplyTemplate indexTemplate ctx >>=
          finish defaultTemplate ctx

stripArgMapLink :: Inline -> Inline
stripArgMapLink (Link _ innerHtml ("#arg-map", _)) = Span mempty innerHtml
stripArgMapLink i = i

stripClosedSwitchBlocks :: [Block] -> [Block]
stripClosedSwitchBlocks (d@(Div (_, classes, _) _):_)
  | "open" `elem` classes = [d]
stripClosedSwitchBlocks bs = bs

stripClosedSwitchSpans :: [Inline] -> [Inline]
stripClosedSwitchSpans (s@(Span (_, classes, _) _):_)
  | "open" `elem` classes = [s]
stripClosedSwitchSpans (Space:s@(Span (_, classes, _) _):_)
  | "open" `elem` classes = [s]
stripClosedSwitchSpans is = is

stripForm :: Block -> Block
stripForm (Div (_, classes, _) _)
  | "form" `elem` classes =
    Para [Emph [Str "On the full site, there's an interactive widget here."]]
stripForm b = b

collectMacros :: Block -> [String]
collectMacros (Div (_, classes, _) [Para [Math DisplayMath macros]])
  | "macros" `elem` classes = [macros]
collectMacros _ = mempty

stripMacro :: Block -> Block
stripMacro (Div (_, classes, _) _)
  | "macros" `elem` classes = Null
stripMacro b = b

data MathRenderMethod
  = MathjaxNode
  | FeedlyCompatible
  | Mock
  deriving (Read)

renderMath ::
     MathRenderMethod -> FilePath -> String -> Inline -> Compiler Inline
renderMath MathjaxNode destinationDir macros (Math typ math) = do
  alreadyRendered <- compilerUnsafeIO . doesPathExist $ destSvg
  unless alreadyRendered $ writeAsSvgFile destSvg macros typ math
  pure $ Image mempty mempty (destination "png", math)
  where
    destSvg = destinationDir <> destination "svg"
    destination ext = "/images/latex/" <> show (hash math) <> "." <> ext
renderMath FeedlyCompatible destinationDir macros (Math DisplayMath math) = do
  alreadyRendered <- compilerUnsafeIO . doesPathExist $ destSvg
  unless alreadyRendered $ writeAsSvgFile destSvg macros DisplayMath math
  pure $ Image mempty mempty (destination "png", math)
  where
    destSvg = destinationDir <> destination "svg"
    destination ext = "/images/latex/" <> show (hash math) <> "." <> ext
renderMath _ _ _ i = pure i

writeAsSvgFile :: FilePath -> String -> MathType -> String -> Compiler ()
writeAsSvgFile destination macros typ math =
  compilerUnsafeIO (findExecutable "tex2svg") >>= \case
    Nothing -> error "Couldn't find `tex2svg`"
    Just tex2svg -> do
      debugCompiler $
        "Calling " <> tex2svg <> " " <>
            -- `phantom` is a very hacky way to avoid having the shell interpret an initial `-` as a command line option
        show (inlineFlag <> [macros <> math])
      svg <-
        compilerUnsafeIO $
        readProcess
          tex2svg
          (inlineFlag <> ["\\phantom{}" <> macros <> math])
          mempty
      compilerUnsafeIO $ writeFile destination svg
  where
    inlineFlag =
      case typ of
        InlineMath -> ["--inline"]
        DisplayMath -> mempty

buildAtom ::
     MathRenderMethod
  -> FilePath
  -> String
  -> Blessed "post" Pattern
  -> Blessed "bib" Identifier
  -> Blessed "csl" Identifier
  -> Rules (Blessed "feed" Identifier)
buildAtom renderMethod destinationDir webHost postPat bibIdent cslIdent =
  (head <$>) . create ["atom.xml"] $ do
    route idRoute
    compile $ do
      bib <- load bibIdent
      csl <- load cslIdent
      loadAllSnapshots postPat "raw" >>= recentFirst >>=
        traverse
          (readPandocBiblio readerOpt csl bib >=>
           traverse markdownTransform >=>
           toHtmlAndBack bib csl >=>
           traverse htmlTransform >=>
           pure . (relativizeUrlsWith webHost <$>) . writePandocWith writerOpt) >>=
        fmap (replaceAll "index.html" (const mempty) <$>) .
        renderAtom feedConfig (postCtx <> bodyField "description")
  where
    markdownTransform p =
      walkM
        (renderMath
           renderMethod
           destinationDir
           (unlines . query collectMacros $ p)) .
      walk stripMacro $
      p
    htmlTransform =
      pure .
      walk stripForm .
      walk stripArgMapLink .
      walk stripClosedSwitchBlocks . walk stripClosedSwitchSpans

-- We set the extension to html to force html parsing and then back to md so that we pick up the right metadata
-- This method is sometimes useful for ensuring that /all/ divs and spans show up in pandoc as native divs and spans
toHtmlAndBack ::
     Item Biblio -> Item CSL -> Item Pandoc -> Compiler (Item Pandoc)
toHtmlAndBack bib csl =
  fmap (setExtension' ".md") .
  readPandocBiblio readerOpt csl bib .
  setExtension' ".html" . writePandocWith writerOpt
  where
    setExtension' ext (Item identifier body) =
      Item
        (fromFilePath . (`replaceExtension` ext) . toFilePath $ identifier)
        body

swapJsForEin :: String -> String
swapJsForEin = replaceAll "#\\+BEGIN_SRC ein" (const "#+BEGIN_SRC js")

includeOrgFiles :: FilePath -> String -> String
includeOrgFiles directory =
  replaceAll "\\[\\[file:.*\\]\\]" (createInclude . extractFileName)
  where
    createInclude fileName =
      "#+INCLUDE: \"" <> directory <> "/" <> fileName <> "\" export html"
    extractFileName =
      reverse .
      fromMaybe (error "Impossible match") .
      (stripPrefix "]]" . reverse <=< stripPrefix "[[file:")

feedConfig :: FeedConfiguration
feedConfig =
  FeedConfiguration
    { feedTitle = "Collectively Exhaustive"
    , feedDescription = "A weblog"
    , feedAuthorName = "Cole Haus"
    , feedAuthorEmail = "colehaus@cryptolab.net"
    , feedRoot = prodHost
    }

buildArchive ::
     Blessed "post" Pattern
  -> Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Rules (Blessed "index" Identifier)
buildArchive postPat defaultTemplate archiveTemplate tags =
  (head <$>) . create ["posts/index.html"] $ do
    route idRoute
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

buildCss :: Blessed "scss" Pattern -> Rules (Blessed "css" Pattern)
buildCss scssPat = do
  includes <- makePatternDependency scssPat
  rulesExtraDependencies [includes] $
    match "css/*.scss" $ do
      route $ setExtension "css"
      compile compileScss

fileInDirectoryRoute :: FilePath -> Rules ()
fileInDirectoryRoute dir =
  route . customRoute $ \ident ->
    dir <> "/" <> (takeFileName . toFilePath) ident

buildPosts ::
     Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Blessed "bib" Identifier
  -> Blessed "csl" Identifier
  -> Tags
  -> Map String [String]
  -> String
  -> Rules ( Blessed "post" Pattern
           , Blessed "warnings" Pattern
           , Blessed "overlay" Pattern
           , Blessed "abstract" Pattern)
buildPosts defaultTemplate postTemplate bibIdent cslIdent tags chunkMap dir = do
  overlayPat <-
    match (fromGlob $ dir <> "/*/overlay.md") . compile $
    pandocCompilerWith readerOpt writerOpt
  overlays <- makePatternDependency overlayPat
  abstractPat <-
    match (fromGlob $ dir <> "/*/abstract.md") . compile $
    pandocCompilerWith readerOpt writerOpt
  abstracts <- makePatternDependency abstractPat
  warningsPat <-
    match (fromGlob $ dir <> "/*/warnings.md") $ compile getResourceBody
  warnings <- makePatternDependency warningsPat
  postPat <-
    rulesExtraDependencies [overlays, warnings, abstracts] $
    match (mkPostPat dir) $ do
      route . customRoute $ \ident ->
        dir <> "/" <> (takeBaseName . takeDirectory . toFilePath) ident <>
        "/index.html"
      compile $
        let ctx =
              warnCtx warningsPat <> overCtx overlayPat <>
              abstractCtx abstractPat <>
              jsCtx chunkMap <>
              cssCtx <>
              tagsCtx tags <>
              customElementsCtx chunkMap <>
              postCtx
         in do bib <- load bibIdent
               csl <- load cslIdent
               directory <- takeDirectory . toFilePath <$> getUnderlying
               getResourceBody >>=
                 saveSnapshot "raw" .
                 fmap (swapJsForEin . includeOrgFiles directory) >>=
                 readPandocBiblio readerOpt csl bib >>=
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
      (unixFilter
         "scss"
         ["--sourcemap=none", "--style=compressed", "--trace", "-I", dir])
      i

mkPostPat :: String -> Pattern
mkPostPat postDir = fromGlob $ postDir <> "/*/main.*"

finish ::
     Blessed "template" Identifier
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
lastPage pg = paginateMakeId pg . Map.size . paginateMap $ pg

listFieldWith' ::
     String -> Context a -> (Identifier -> Compiler [a]) -> Context b
listFieldWith' k ctx f =
  listFieldWith k ctx $ (traverse makeItem =<<) . f . itemIdentifier

constListField :: String -> Context a -> [a] -> Context b
constListField key1 ctx list =
  listFieldWith key1 ctx $ const (traverse makeItem list)

idField :: String -> Context String
idField key = field key $ pure . itemBody

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

customElementsCtx :: Map String [String] -> Context String
customElementsCtx chunkMap =
  constListField "custom-elements" (idField "filename") $
  "custom-elements" : chunkMap Map.! "custom-elements"

jsCtx :: Map String [String] -> Context String
jsCtx chunkMap =
  listFieldWith' "jses" (idField "filename") $
  fmap (>>= correspondingChunks) . getListMeta "js"
  where
    correspondingChunks str =
      maybe (pure str) (str :) $ str `Map.lookup` chunkMap

cssCtx :: Context String
cssCtx = listFieldWith' "csses" (idField "filename") $ getListMeta "css"

getListMeta :: MonadMetadata m => String -> Identifier -> m [String]
getListMeta k ident =
  maybe mempty (map trim . splitAll ",") . lookupString k <$> getMetadata ident

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
readerOpt = defaultHakyllReaderOptions {readerExtensions = extensions}

writerOpt :: WriterOptions
writerOpt =
  defaultHakyllWriterOptions
    -- Hakyll + Pandoc don't insert <script> correctly,
    -- so we add dummy URL and do it manually
    {writerHTMLMathMethod = MathJax mempty, writerExtensions = extensions}

perPage :: Int
perPage = 5
