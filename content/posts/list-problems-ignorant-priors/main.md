---
title: A non-exhaustive list of putative problems with ignorant priors
date: 2018-08-17
edited: 2018-11-14
tags: bayes, epistemology
---

The [principle of indifference](https://en.wikipedia.org/wiki/Principle_of_indifference) (a.k.a. the principle of insufficient reason) suggests that when considering a set of possibilities and there's no known reason for granting special credence to one possibility, we ought to assign all possibilities the same credence (which, on the [Bayesian](https://plato.stanford.edu/entries/epistemology-bayesian/) point of view, is also a probability). For example, when someone asks what the result of a 6-sided die roll is, the principle of indifference recommends we assign a probability of 1/6 to each outcome. Slightly more interesting is that it *also* recommends assigning a probability of 1/6 to each outcome even when we're told the die is weighted as long as we're not told how it's weighted.

There's a definite intuitive plausibility and appeal to this rule. But it turns out there are a lot of difficulties when it comes to actually operationalizing it. Below, I list some of the problems that have been raised over the years. Some of these problems seem silly to me and will doubtless seem silly to you. Others strike me as important. I list them all here regardless and ignore any claimed solutions for the moment.

# Coarsening

"What is the origin country of this unknown traveler? France, Ireland or Great Britain?"

Naive application of the principle of indifference (NAPI) suggests we assign probability 1/3 to each possibility.

The question can be rephrased: "What is the origin country of this unknown traveler? France, or the British Isles?".

In this case, <abbr title="Naive application of the principle of indifference">NAPI</abbr> suggests we ought to assign a probability of 1/2 to each possibility.

So, depending on the framing, we assign probability 1/2 or 1/3 to the same outcome---the traveler is from France.

| Description                       | Outcome | Probability |
|:----------------------------------|:--------|:------------|
| France, Ireland, or Great Britain | France  | 1/3         |
| France or British Isles           | France  | 1/2         |

# Negation

"I've just pulled a colored ball from an urn containing an equal number of red, black and yellow balls. Which color is the ball? Red, black, or yellow?"

<abbr title="Naive application of the principle of indifference">NAPI</abbr> suggests we assign probability 1/3 to each possibility.

The question can be rephrased: "Which color is the ball? Red or not red?".

In this case, <abbr title="Naive application of the principle of indifference">NAPI</abbr> suggests we ought to assign a probability 1/2 to each possibility.

So, depending on the framing, we assign probability 1/2 or 1/3 to the same outcome---the ball is red.

| Description          | Outcome | Probability |
|:---------------------|:--------|:------------|
| Red, black or yellow | Red     | 1/3         |
| Red or not red       | Red     | 1/2         |

# [Bertrand paradox](https://en.wikipedia.org/wiki/Bertrand_paradox_(probability))

"I have an equilateral triangle inscribed in a circle. I've also chosen a chord in the circle randomly. What is the probability that the chord is longer than a side of the triangle?"

If we construct our random chords by choosing two random points on the circumference of the circle and construct a chord between them, we find that the probability of a long chord is 1/3.

If we construct our random chords by choosing a random radius and then constructing a chord perpendicular to a random point on that radius, we find that the probability of a long chord is 1/2.

If we construct our random chords by choosing a random point inside the circle and constructing a chord with that point as its midpoint, we find that the probability of a long chord is 1/4.

So depending on our framing, we assign probability of 1/4, 1/3 or 1/2 to the same proposition.

| Description                          | Outcome    | Probability |
|:-------------------------------------|:-----------|:------------|
| Chord by two points on circumference | Long chord | 1/3         |
| Chord by random radius and point     | Long chord | 1/2         |
| Chord by random point as midpoint    | Long chord | 1/4         |

<!--more-->

# Cube

"I'm holding a perfect cube behind my back. It's sides are between 0 cm and 2 cm. What is the probability that its sides are less than 1 cm?"

If we straightforwardly apply the principle of indifference and assign a uniform probability distribution over all possible cube side lengths, the probability is 1/2.

If we apply the principle of indifference and assign a uniform probability distribution over all possible cube surface areas, the probability is 1/4.

If we apply the principle of indifference and assign a uniform probability distribution over all possible cube volumes, the probability is 1/8.

So depending on our framing, we assign probability of 1/8, 1/4 or 1/2 to the same proposition.

| Description                    | Outcome     | Probability |
|:-------------------------------|:------------|:------------|
| Indifferent over side lengths  | Short sides | 1/2         |
| Indifferent over surface areas | Short sides | 1/4         |
| Indifferent over volumes       | Short sides | 1/8         |

# Water and wine

"I have a goblet of mixed water and wine. The ratio of wine to water is between 1/3 and 3. What is the probability that the ratio of wine to water is less than 2?"

If we apply the principle of indifference over the possible ratios of wine to water, the probability is 5/8.

If we apply the principle of indifference over the possible ratios of water to wine, the probability is 15/16.

So depending on our framing, we assign probability of 5/8 and 15/16 to the same proposition.

| Description                 | Outcome        | Probability |
|:----------------------------|:---------------|:------------|
| Indifferent over wine:water | Dilute mixture | 5/8         |
| Indifferent over water:wine | Dilute mixture | 15/16       |

# Inductive disjunction fallacy

There is only one way for [Nothing]{.small-caps} to exist, but there are many possible ways for something to exist. By assigning equal probability to all these [Nothing]{.small-caps} and something possibilities, the principle of indifference explains why [Nothing]{.small-caps} is improbable and why the existence of the universe is probable. [@van1996]

<hr class="references">

