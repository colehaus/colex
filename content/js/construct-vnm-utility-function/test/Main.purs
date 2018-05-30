module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Set as Set
import Economics.Utility.VNM.Function (goodsToInitialFn)
import Economics.Utility.VNM.Helpers (unsafeFromJustBecause)
import Helpers (nonEmptySet)
import Main (update)
import Test.QuickCheck (arbitrary, quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION , random :: RANDOM | e) Unit
main = do
  Console.log "Doesn't crash"
  quickCheckGen' 1000 $ runUpdates <$> genOrderingList

runUpdates :: NonEmpty Array Ordering -> Boolean
runUpdates ords = true
  where
    initialFn =
      unsafeFromJustBecause "static" <<<
      hush <<<
      goodsToInitialFn <<< unsafeFromJustBecause "static" <<< nonEmptySet $
      Set.fromFoldable ["a", "b", "c"]
    _ =
      foldl (\acc evt -> update evt =<< acc) (Right initialFn) <<<
      map Right <<< NonEmpty.fromNonEmpty Array.cons $
      ords

genOrderingList :: Gen (NonEmpty Array Ordering)
genOrderingList = Gen.arrayOf1 arbitrary
