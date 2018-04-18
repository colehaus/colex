module Data where

import Prelude

import Data.Argonaut.Encode as Argonaut
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Array as Array
import Data.Generic (class Generic, gShow)
import Data.List as List
import Data.Tuple (Tuple(..))

newtype DistEntry a n =
  MkDistEntry { outcome :: a, prob :: n }
derive instance genericDistEntry :: (Generic a, Generic n) => Generic (DistEntry a n)
unwrapDistEntry (MkDistEntry { outcome, prob }) = Tuple outcome prob
wrapDistEntry (Tuple outcome prob) = MkDistEntry { outcome, prob }

newtype Result l v =
  MkResult { label :: l, value :: v }
derive instance eqResult :: (Eq l, Eq v) => Eq (Result l v)
derive instance ordResult :: (Ord l, Ord v) => Ord (Result l v)
derive instance genericResult :: (Generic l, Generic v) => Generic (Result l v)
instance showResult :: (Generic l, Generic v) => Show (Result l v) where
  show = gShow
instance encodeResult :: (Generic l, Generic v) => Argonaut.EncodeJson (Result l v) where
  encodeJson = encodeJson

newtype Choice c r n =
  MkChoice { choice :: c, results :: Array (DistEntry r n) }
unwrapChoice (MkChoice { choice, results }) =
  Tuple choice (unwrapDistEntry <$> List.fromFoldable results)
wrapChoice (Tuple choice results) =
  MkChoice { choice, results: (wrapDistEntry <$> Array.fromFoldable results) }
derive instance genericChoice ::
     (Generic c, Generic r, Generic n)
  => Generic (Choice c r n)

newtype Outcome f c r n =
  MkOutcome { finding :: f, choices :: Array (Choice c r n) }
derive instance genericOutcome ::
     (Generic f, Generic c, Generic r, Generic n)
  => Generic (Outcome f c r n)
unwrapOutcome (MkOutcome { finding, choices }) =
  Tuple finding (unwrapChoice <$> List.fromFoldable choices)
wrapOutcome (Tuple finding choices) = MkOutcome { finding, choices: wrapChoice <$> Array.fromFoldable choices }

newtype Investigation p f c r n =
  MkInvestigation (Array (DistEntry (Outcome f c r n) n))
derive instance genericInvestigation ::
     (Generic p, Generic f, Generic c, Generic r, Generic n)
  => Generic (Investigation p f c r n)
instance showInvestigation ::
     (Generic p, Generic f, Generic c, Generic r, Generic n)
  => Show (Investigation p f c r n) where
  show = gShow
unwrapInvestigation (MkInvestigation investigation) =
  first unwrapOutcome <<< unwrapDistEntry <$> List.fromFoldable investigation
wrapInvestigation investigation =
  wrapDistEntry <<< first wrapOutcome <$> Array.fromFoldable investigation

first :: forall a b c. (a -> c) -> Tuple a b -> Tuple c b
first f (Tuple a b) = Tuple (f a) b
