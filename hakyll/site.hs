{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (empty)
import Control.Monad ((<=<), (>=>), unless)
import Control.Monad.Trans.Class (lift)
import Data.Functor ((<$>))
import Data.Hashable (hash)
import Data.List (sortOn, foldl1', stripPrefix)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Ord (Down(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
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
  , takeExtension
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
import Hakyll.Series
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
    tags <- buildTags (mkPostPat "posts") (fromCapture "tag/*/index.html" . toUrl)
    series <- buildSeries (mkPostPat "posts") (fromCapture "series/*/index.html" . toUrl)
    chunkMap <- unsafeRules buildChunkMap
    (postPat, _, _, abstractPat) <-
      buildPosts
        defaultTemplate
        (narrowEx templates "templates/post.html")
        bib
        csl
        tags
        series
        chunkMap
        "posts"
    _ <-
      buildPosts
        defaultTemplate
        (narrowEx templates "templates/post.html")
        bib
        csl
        tags
        series
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
    buildTagPages "Tagged" defaultTemplate (narrowEx templates "templates/tag.html") tags tags
    buildTagPages "Series:" defaultTemplate (narrowEx templates "templates/tag.html") series tags
    paginate <-
      buildPaginateWith
        ((paginateEvery perPage <$>) . sortChronological)
        (unBlessedPattern postPat)
        (fromCapture "page/*/index.html" . show)
    buildPages
      defaultTemplate
      (narrowEx templates "templates/index.html")
      (narrowEx templates "templates/index-content.html")
      abstractPat
      tags
      series
      paginate
    tagIndexIdent <-
      buildTagIndex
        defaultTemplate
        (narrowEx templates "templates/tags.html")
        tags
    seriesIndexIdent <-
      buildSeriesIndex
        defaultTemplate
        (narrowEx templates "templates/series-index.html")
        series
    indexIdent <-
      buildIndex
        defaultTemplate
        (narrowEx templates "templates/index.html")
        (narrowEx templates "templates/index-content.html")
        abstractPat
        tags
        series
        paginate
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
        seriesIndexIdent
        indexIdent
        archiveIdent
    pure ()

purescriptProjects :: [Pattern]
purescriptProjects =
  [ "js/dist/bibliometric.js"
  , "js/dist/value-of-information-calculator.js"
  , "js/dist/construct-vnm-utility-function.js"
  , "js/dist/exemplars-curse.js"
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
    Nothing -> error $ fp <> ": Not a vendor chunk"

prodHost :: String
prodHost = "https://www.col-ex.org"

buildSitemap ::
     Blessed "template" Identifier
  -> Blessed "post" Pattern
  -> Blessed "feed" Identifier
  -> Blessed "index" Identifier
  -> Blessed "index" Identifier
  -> Blessed "index" Identifier
  -> Blessed "index" Identifier
  -> Rules (Blessed "map" Identifier)
buildSitemap sitemapTemplate postsPat feedIdent tagIndexIdent seriesIndexIdent indexIdent archiveIdent =
  (head <$>) . create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPat
      feed <- load feedIdent
      tagIndex <- load tagIndexIdent
      seriesIndex <- load seriesIndexIdent
      index <- load indexIdent
      archive <- load archiveIdent
      tags <- Hakyll.loadAll "tag/*/index.html"
      series <- Hakyll.loadAll "series/*/index.html"
      pages <- Hakyll.loadAll "page/*/index.html"
      makeItem mempty >>=
        loadAndApplyTemplate
          sitemapTemplate
          (mconcat
             [ listField
                 "entries"
                 (postCtx <> constField "host" prodHost)
                 (pure $
                  feed :
                  tagIndex :
                  seriesIndex :
                  index : archive : posts <> tags <> series <> pages)
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
  -> Tags
  -> Paginate
  -> Rules ()
buildPages defaultTemplate indexTemplate indexContentTemplate abstractPat tags series pages =
  paginateRules pages $ \n pat -> do
    route idRoute
    compile $
      let pageCtx = paginateContext pages n
          ctx =
            constField "title" ("Page " <> show n) <>
            listField
              "posts"
              (teaserField "teaser" "teaser" <> tagsCtx tags <> seriesCtx series <>
               pageCtx <>
               abstractCtx abstractPat <>
               postCtx)
              (recentFirst =<< Hakyll.loadAll pat) <>
            pageCtx <>
            defaultContext
       in makeItem mempty >>= loadAndApplyTemplate indexContentTemplate ctx >>=
          saveSnapshot "page" >>=
          loadAndApplyTemplate indexTemplate ctx >>=
          finish defaultTemplate ctx

buildTagPages ::
     String
  -> Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Tags
  -> Rules ()
buildTagPages titleText defaultTemplate tagTemplate tags postTags =
  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $
      let ctx =
            constField "title" (titleText <> " " <> tag) <>
            boolField (tag <> "-page") (const True) <>
            listField
              "posts"
              (tagsCtx postTags <> postCtx)
              (recentFirst =<< Hakyll.loadAll pat) <>
            defaultContext
       in makeItem mempty >>= loadAndApplyTemplate tagTemplate ctx >>=
          finish defaultTemplate ctx

buildSeriesIndex ::
     Blessed "template" Identifier
  -> Blessed "template" Identifier
  -> Tags
  -> Rules (Blessed "index" Identifier)
buildSeriesIndex defaultTemplate seriesIndexTemplate series =
  (head <$>) . create ["series/index.html"] $ do
    route idRoute
    compile $ do
      let ctx =
            seriesListCtx series <> constField "title" "Series" <>
            boolField "series-index-page" (const True) <>
            defaultContext
      makeItem mempty >>= loadAndApplyTemplate seriesIndexTemplate ctx >>=
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
  -> Blessed "template" Identifier
  -> Blessed "abstract" Pattern
  -> Tags
  -> Tags
  -> Paginate
  -> Rules (Blessed "index" Identifier)
buildIndex defaultTemplate indexTemplate indexContentTemplate abstractPat tags series pages =
  (head <$>) . create ["index.html"] $ do
    route idRoute
    compile $ do
      pageCtx <-
        case fst ultimatePage of
          1 -> pure mempty
          2 -> pure mempty
          n -> do
            first <- constField "firstPageUrl" <$> pageUrl 1
            prev <- constField "previousPageUrl" <$> pageUrl (n - 2)
            pure $ first <> prev
      let ctx =
            boolField "home-page" (const True) <> constField "title" "Home" <>
            listField
              "posts"
              (teaserField "teaser" "teaser" <> tagsCtx tags <> seriesCtx series <>
               pageCtx <>
               abstractCtx abstractPat <>
               postCtx)
              (recentFirst =<< Hakyll.loadAll pat) <>
            pageCtx <>
            defaultContext
       in makeItem mempty >>= loadAndApplyTemplate indexContentTemplate ctx >>=
          loadAndApplyTemplate indexTemplate ctx >>=
          finish defaultTemplate ctx
  where
    pageUrl n =
      fmap (maybe (error "Missing page " <> show n) toUrl) .
      getRoute . paginateMakeId pages $
      n
    ultimatePage =
      fromMaybe (error "Must have at least one post") $
      Map.lookupMax (paginateMap pages)
    penultimatePage = Map.lookupLT (fst ultimatePage) (paginateMap pages)
    pat =
      fromList . mappend (snd ultimatePage) . maybe mempty snd $ penultimatePage

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
  pure $ Image mempty mempty (destination "svg", math)
  where
    destSvg = destinationDir <> destination "svg"
    destination ext = "/images/latex/" <> show (hash math) <> "." <> ext
renderMath FeedlyCompatible destinationDir macros (Math DisplayMath math) = do
  alreadyRendered <- compilerUnsafeIO . doesPathExist $ destSvg
  unless alreadyRendered $ writeAsSvgFile destSvg macros DisplayMath math
  pure $ Image mempty mempty (destination "svg", math)
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
        show (inlineFlag <> [macros <> math])
      svg <-
        compilerUnsafeIO $
        readProcess
          tex2svg
            -- `phantom` is a very hacky way to avoid having the shell interpret an initial `-` as a command line option
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
           ifMarkdown (toHtmlAndBack bib csl) >=>
           traverse htmlTransform >=>
           pure . (relativizeUrlsWith webHost <$>) . writePandocWith writerOpt) >>=
        fmap (replaceAll "index.html" (const mempty) <$>) .
        renderAtom feedConfig (postCtx <> bodyField "description")
  where
    ifMarkdown f p =
      if takeExtension (toFilePath $ itemIdentifier p) == ".md"
      then f p
      else pure p
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

addCiteLinkDirective :: String -> String
addCiteLinkDirective = replaceAll "---\ntitle:" (const "---\nlink-citations: true\ntitle:")

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
  rulesExtraDependencies [includes] . match "css/*.scss" $ do
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
  -> Tags
  -> Map String [String]
  -> String
  -> Rules ( Blessed "post" Pattern
           , Blessed "warnings" Pattern
           , Blessed "overlay" Pattern
           , Blessed "abstract" Pattern)
buildPosts defaultTemplate postTemplate bibIdent cslIdent tags series chunkMap dir = do
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
    rulesExtraDependencies [overlays, warnings, abstracts] .
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
              seriesCtx series <>
              tagsCtx tags <>
              customElementsCtx chunkMap <>
              graphContentsCtx chunkMap <>
              postCtx
         in do bib <- load bibIdent
               csl <- load cslIdent
               directory <- takeDirectory . toFilePath <$> getUnderlying
               getResourceString >>=
                 saveSnapshot "raw" .
                 fmap (addCiteLinkDirective . swapJsForEin . includeOrgFiles directory) >>=
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
  functionField "dateInWords" (wrap dateInWords) <>
  functionField "dateInNumbers" (wrap dateInNumbers) <>
  dateField "date" "%B %e, %Y" <> dateField "num-date" "%F" <> defaultContext
  where
    wrap :: Applicative m => (a -> c) -> [a] -> b -> m c
    wrap f = flip . const . singleArg $ pure . f

singleArg :: (a -> b) -> [a] -> b
singleArg f [a] = f a
singleArg _ _ = error "Expected single argument to function"

dateInWords :: String -> String
dateInWords = dateIn "%B %e, %Y"

dateInNumbers :: String -> String
dateInNumbers = dateIn "%F"

dateIn :: String -> String -> String
dateIn f s =
  formatTime defaultTimeLocale f .
  fromMaybe (error $ "Bad date " <> s) .
  parseTimeM @_ @UTCTime True defaultTimeLocale "%Y-%m-%d" $ s




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

graphContentsCtx :: Map String [String] -> Context String
graphContentsCtx chunkMap =
  listFieldWith' "arg-map" (idField "filename") $
  fmap (maybe [] (const $ "arg-map" : chunkMap Map.! "arg-map")) . flip getMetadataField "graph-of-contents"

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
  maybe mempty (fmap trim . splitAll ",") . lookupString k <$> getMetadata ident

seriesListCtx :: Tags -> Context String
seriesListCtx series =
  listField "series-list" seriesCtx' .
  (traverse makeItem . latestMostRecentFirst <=< addDates) $
  tagsMap series
  where
    latestMostRecentFirst = sortOn (Down . maximum . fmap snd . snd)
    addDates :: [(t, [Identifier])] -> Compiler [(t, [(Identifier, UTCTime)])]
    addDates =
      traverse (secondM (traverse (appM (getItemUTC defaultTimeLocale))))
    seriesCtx' =
      field "series-name" (pure . fst . itemBody) <>
      field "series-url" (pure . toUrl . toFilePath . tagsMakeId series . fst . itemBody) <>
      field "series-size" (pure . show . length . snd . itemBody) <>
      field
        "series-start"
        (pure .
         formatTime defaultTimeLocale "%y-%m-%d" . minimum . fmap snd . snd . itemBody) <>
      field
        "series-end"
        (pure .
         (formatTime defaultTimeLocale "%y-%m-%d" .
          maximum . fmap snd . snd . itemBody))
    secondM f (a, b) = (a, ) <$> f b
    appM f x = (x, ) <$> f x

seriesCtx :: Tags -> Context String
seriesCtx series =
  field "series-present" (maybe empty pure <=< getSeries . itemIdentifier) <>
  field "series-name" (maybe empty pure <=< getSeries . itemIdentifier) <>
  field
    "series-url"
    (maybe empty (pure . toUrl . toFilePath . tagsMakeId series) <=<
     getSeries . itemIdentifier)

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
