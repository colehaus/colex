{-# LANGUAGE LambdaCase #-}

module HakyllPandoc where

import Hakyll hiding (readPandocWith)
import qualified Data.Text as T
import qualified Text.CSL as CSL
import Text.CSL.Pandoc
import Text.Pandoc

readPandocBiblio
  :: ReaderOptions
  -> Item CSL
  -> Item Biblio
  -> Item String
  -> Compiler (Item Pandoc)
readPandocBiblio ropt csl biblio item
    -- Parse CSL file, if given
 = do
  style <-
    unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl
    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
  let Biblio refs = itemBody biblio
  pandoc <- itemBody <$> readPandocWith ropt item
  let pandoc' = processCites style refs pandoc
  return $ fmap (const pandoc') item

readPandocWith
  :: ReaderOptions -- ^ Parser options
  -> Item String -- ^ String to read
  -> Compiler (Item Pandoc) -- ^ Resulting document
readPandocWith ropt item =
  unsafeCompiler $
  runIO (traverse (reader ropt (itemFileType item)) (fmap T.pack item)) >>= \case
    Left err ->
      fail $ "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
    Right item' -> return item'
  where
    reader ro t =
      case t of
        DocBook -> readDocBook ro
        Html -> readHtml ro
        LaTeX -> readLaTeX ro
        LiterateHaskell t' -> reader (addExt ro Ext_literate_haskell) t'
        Markdown -> readMarkdown ro
        MediaWiki -> readMediaWiki ro
        OrgMode -> readOrg ro
        Rst -> readRST ro
        Textile -> readTextile ro
        _ ->
          error $
          "Hakyll.Web.readPandocWith: I don't know how to read a file of " ++
          "the type " ++ show t ++ " for: " ++ show (itemIdentifier item)
    addExt ro e =
      ro {readerExtensions = enableExtension e $ readerExtensions ro}
