{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module HakyllExtension where

import Data.Binary
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Hakyll hiding (load, match)
import qualified Hakyll

data Blessed a where
  MkBlessedIdentifier :: { unBlessedIdentifier :: Identifier } -> Blessed Identifier
  MkBlessedPattern :: { unBlessedPattern :: Pattern } -> Blessed Pattern
deriving instance (Show a) => Show (Blessed a)

narrow :: Blessed Pattern -> Identifier -> Maybe (Blessed Identifier)
narrow (MkBlessedPattern pat) ident
  | pat `matches` ident = Just (MkBlessedIdentifier ident)
  | otherwise = Nothing

narrowEx :: Blessed Pattern -> Identifier -> Blessed Identifier
narrowEx pat ident =
  fromMaybe (error $ "Couldn't match " <> show ident <> " with " <> show pat) $
  narrow pat ident

match :: Pattern -> Rules () -> Rules (Blessed Pattern)
match pat rules = MkBlessedPattern pat <$ Hakyll.match pat rules

matchIdentifier :: Identifier -> Rules () -> Rules (Blessed Identifier)
matchIdentifier ident rules =
  MkBlessedIdentifier ident <$ Hakyll.match (identifierToPattern ident) rules

identifierToPattern :: Identifier -> Pattern
identifierToPattern = fromGlob . toFilePath

load :: (Typeable a, Binary a) => Blessed Identifier -> Compiler (Item a)
load = Hakyll.load . unBlessedIdentifier

loadBody :: (Typeable a, Binary a) => Blessed Identifier -> Compiler a
loadBody = Hakyll.loadBody . unBlessedIdentifier

loadAndApplyTemplate :: Blessed Identifier -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplate = Hakyll.loadAndApplyTemplate . unBlessedIdentifier

loadAll :: (Typeable a, Binary a) => Blessed Pattern -> Compiler [Item a]
loadAll = Hakyll.loadAll . unBlessedPattern
