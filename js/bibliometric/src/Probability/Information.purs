module Probability.Information where

import Math

import Probability

newtype Entropy = Entropy Number
entropyNum = Iso (\(Entropy e) -> e) Entropy

nonCond :: forall a b c. ((Dist Unit) -> (Unit -> Dist b) -> c) -> Dist b -> c
nonCond f d = f (pure unit) (const d)

entropy :: forall x z. (Eq x) => Dist z -> (z -> Dist x) -> Entropy
entropy zs x'zs = expected entropyNum $ do
  z <- zs
  x'z <- x'zs z
  let px'z = just x'z ?? x'zs z
  pure $ selfInformation px'z

pointwiseInformation :: Prob -> Prob -> Prob -> Entropy
pointwiseInformation pxy'z px'z py'z =
  from entropyNum $ log2 (xy / (x * y)) where
    xy = runProb pxy'z
    x = runProb px'z
    y = runProb py'z

mutualInformation ::
  forall j x y z. (Eq x, Eq y, Eq j) =>
  Dist z -> (z -> Dist j) -> (j -> x) -> (j -> y)  -> Entropy
mutualInformation zs xys'z jx jy = expected entropyNum $ do
  z <- zs
  xy'z <- xys'z z
  pure $ pointwiseInformation (just xy'z ?? xys'z z)
                              ((==) (jx xy'z) <<< jx ?? xys'z z)
                              ((==) (jy xy'z) <<< jy ?? xys'z z)

log2 :: Number -> Number
log2 = logBase 2

logBase :: Number -> Number -> Number
logBase b n = log n / log b

selfInformation :: Prob -> Entropy
selfInformation = from entropyNum <<< negate <<< log2 <<< runProb

divergence :: forall x z. (Eq x) =>
              Dist z -> (z -> Dist x) -> (z -> Dist x) -> Entropy
divergence zs x'zs y'zs = expected entropyNum $ do
  z <- zs
  x'z <- x'zs z
  let px'z = just x'z ?? x'zs z
  let py'z = just x'z ?? y'zs z
  pure <<< from entropyNum <<< log2 $ runProb px'z / runProb py'z

instance eqEnt :: Eq Entropy where
  (==) (Entropy a) (Entropy b) = a == b
  (/=) a b = not $ a == b
instance ordEnt :: Ord Entropy where
  compare (Entropy a) (Entropy b) = compare a b
instance showEnt :: Show Entropy where
  show (Entropy n) = "Entropy " <> show n
