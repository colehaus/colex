module Data where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject, caseJsonObject, caseJsonArray)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

newtype DistEntry a n =
  MkDistEntry { outcome :: a, prob :: n }
derive instance genericDistEntry :: Generic (DistEntry a n) _
instance showDistEntry :: (Show a, Show n) => Show (DistEntry a n) where
  show = genericShow
unwrapDistEntry (MkDistEntry { outcome, prob }) = Tuple outcome prob
wrapDistEntry (Tuple outcome prob) = MkDistEntry { outcome, prob }
encodeDistEntryJson encodeProbJson (MkDistEntry { outcome, prob }) =
  "outcome" := outcome ~>
  "prob" := encodeProbJson prob ~>
  jsonEmptyObject
decodeDistEntryJson decodeProbJson = caseJsonObject (Left "Expected an Object") (\obj -> do
  outcome <- obj .? "outcome"
  probRaw <- obj .? "prob"
  prob <- decodeProbJson probRaw
  pure $ MkDistEntry { prob, outcome })
decodeDistEntryOutcomeJson decodeProbJson = caseJsonObject (Left "Expected an Object") (\obj -> do
  outcomeRaw <- obj .? "outcome"
  outcome <- decodeOutcomeJson decodeProbJson outcomeRaw
  probRaw <- obj .? "prob"
  prob <- decodeProbJson probRaw
  pure $ MkDistEntry { prob, outcome })

newtype Result l v =
  MkResult { label :: l, value :: v }
derive instance eqResult :: (Eq l, Eq v) => Eq (Result l v)
derive instance ordResult :: (Ord l, Ord v) => Ord (Result l v)
derive instance genericResult :: Generic (Result l v) _
instance showResult :: (Show l, Show v) => Show (Result l v) where
  show = genericShow
instance encodeResult :: (EncodeJson l, EncodeJson v) => EncodeJson (Result l v) where
  encodeJson (MkResult { label, value }) =
    "label" := label ~>
    "results" := value ~>
    jsonEmptyObject
instance decodeResult :: (DecodeJson l, DecodeJson v) => DecodeJson (Result l v) where
  decodeJson = caseJsonObject (Left "Expected an Object") (\obj -> do
    label <- obj .? "label"
    value <- obj .? "value"
    pure $ MkResult { label, value })

newtype Choice c r n =
  MkChoice { choice :: c, results :: Array (DistEntry r n) }
unwrapChoice (MkChoice { choice, results }) =
  Tuple choice (unwrapDistEntry <$> List.fromFoldable results)
wrapChoice (Tuple choice results) =
  MkChoice { choice, results: (wrapDistEntry <$> Array.fromFoldable results) }
derive instance genericChoice :: Generic (Choice c r n) _
instance showChoice :: (Show c, Show r, Show n) => Show (Choice c r n) where
  show = genericShow
encodeChoiceJson encodeProbJson (MkChoice { choice, results }) =
  "choice" := choice ~>
  "results" := (encodeDistEntryJson encodeProbJson <$> results) ~>
  jsonEmptyObject
decodeChoiceJson decodeProbJson = caseJsonObject (Left "Expected an Object") (\obj -> do
  choice <- obj .? "choice"
  resultsRaw <- obj .? "results"
  results <- traverse (decodeDistEntryJson decodeProbJson) resultsRaw
  pure $ MkChoice { choice, results })

newtype Outcome f c r n =
  MkOutcome { finding :: f, choices :: Array (Choice c r n) }
derive instance genericOutcome :: Generic (Outcome f c r n) _
instance showOutcome :: (Show f, Show c, Show r, Show n) => Show (Outcome f c r n) where
  show = genericShow
decodeOutcomeJson decodeProbJson = caseJsonObject (Left "Expected an Object") (\obj -> do
  finding <- obj .? "finding"
  choicesRaw <- obj .? "choices"
  choices <- traverse (decodeChoiceJson decodeProbJson) choicesRaw
  pure $ MkOutcome { finding, choices})
unwrapOutcome (MkOutcome { finding, choices }) =
  Tuple finding (unwrapChoice <$> List.fromFoldable choices)
wrapOutcome (Tuple finding choices) = MkOutcome { finding, choices: wrapChoice <$> Array.fromFoldable choices }

newtype Investigation f c r n =
  MkInvestigation (Array (DistEntry (Outcome f c r n) n))
derive instance genericInvestigation :: Generic (Investigation f c r n) _
instance showInvestigation ::
     (Show f, Show c, Show r, Show n)
  => Show (Investigation f c r n) where
  show = genericShow
unwrapInvestigation (MkInvestigation investigation) =
  first unwrapOutcome <<< unwrapDistEntry <$> List.fromFoldable investigation
wrapInvestigation investigation =
  wrapDistEntry <<< first wrapOutcome <$> Array.fromFoldable investigation
decodeInvestigationJson decodeProbJson =
  caseJsonArray (Left "Expected an Array") (map MkInvestigation <<< traverse (decodeDistEntryOutcomeJson decodeProbJson))

first :: forall a b c. (a -> c) -> Tuple a b -> Tuple c b
first f (Tuple a b) = Tuple (f a) b
