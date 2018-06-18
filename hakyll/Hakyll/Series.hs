{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Series where

import           Data.Maybe                     (maybeToList)

import           Hakyll.Core.Identifier         (Identifier)
import           Hakyll.Core.Identifier.Pattern (Pattern)
import           Hakyll.Core.Metadata           (MonadMetadata, getMetadata,
                                                 lookupString)
import           Hakyll.Web.Tags                (Tags, buildTagsWith)


getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries identifier = lookupString "series" <$> getMetadata identifier

buildSeries :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildSeries = buildTagsWith (fmap maybeToList . getSeries)
