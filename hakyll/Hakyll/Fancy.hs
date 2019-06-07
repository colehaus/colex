{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hakyll.Fancy where

import Data.Binary
import Data.Maybe
import Data.Typeable
import GHC.TypeLits
import Hakyll hiding (load, match)
import qualified Hakyll

data Blessed (ty :: Symbol) a where
  MkBlessedIdentifier
    :: { unBlessedIdentifier :: Identifier}
    -> Blessed ty Identifier
  MkBlessedPattern :: { unBlessedPattern :: Pattern} -> Blessed ty Pattern

deriving instance (Show a) => Show (Blessed ty a)

narrow :: Blessed ty Pattern -> Identifier -> Maybe (Blessed ty Identifier)
narrow (MkBlessedPattern pat) ident
  | pat `matches` ident = Just (MkBlessedIdentifier ident)
  | otherwise = Nothing

narrowEx :: Blessed ty Pattern -> Identifier -> Blessed ty Identifier
narrowEx pat ident =
  fromMaybe (error $ "Couldn't match " <> show ident <> " with " <> show pat) $
  narrow pat ident

match :: Pattern -> Rules () -> Rules (Blessed ty Pattern)
match pat rules = MkBlessedPattern pat <$ Hakyll.match pat rules

matchIdentifier :: Identifier -> Rules () -> Rules (Blessed ty Identifier)
matchIdentifier ident rules =
  MkBlessedIdentifier ident <$ Hakyll.match (identifierToPattern ident) rules

create :: [Identifier] -> Rules () -> Rules [Blessed ty Identifier]
create idents rules =
  MkBlessedIdentifier <$> idents <$ Hakyll.create idents rules

identifierToPattern :: Identifier -> Pattern
identifierToPattern = fromGlob . toFilePath

load :: (Typeable a, Binary a) => Blessed ty Identifier -> Compiler (Item a)
load = Hakyll.load . unBlessedIdentifier

loadBody :: (Typeable a, Binary a) => Blessed ty Identifier -> Compiler a
loadBody = Hakyll.loadBody . unBlessedIdentifier

loadSnapshotBody :: (Typeable a, Binary a) => Blessed ty Identifier -> Snapshot -> Compiler a
loadSnapshotBody ident snap = flip Hakyll.loadSnapshotBody snap . unBlessedIdentifier $ ident

loadAndApplyTemplate ::
     Blessed "template" Identifier
  -> Context a
  -> Item a
  -> Compiler (Item String)
loadAndApplyTemplate = Hakyll.loadAndApplyTemplate . unBlessedIdentifier

loadAll :: (Typeable a, Binary a) => Blessed ty Pattern -> Compiler [Item a]
loadAll = Hakyll.loadAll . unBlessedPattern

loadAllSnapshots ::
     (Typeable a, Binary a)
  => Blessed ty Pattern
  -> Snapshot
  -> Compiler [Item a]
loadAllSnapshots = Hakyll.loadAllSnapshots . unBlessedPattern

makePatternDependency :: (MonadMetadata m) => Blessed ty Pattern -> m Dependency
makePatternDependency = Hakyll.makePatternDependency . unBlessedPattern
