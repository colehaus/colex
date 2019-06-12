---
title: Dominated decisions
series: An Introduction to Decision Theory
published: 2019-01-17
edited: 2019-06-10
tags: decision theory, interactive, yaas
js: decision-demos
css: decision-demos
---

# Decision matrices

[@peterson2017introduction] points out that we can represent decisions with decision matrices. For example, when considering the purchase of home insurance, we have:

<figure>
<figcaption>A decision matrix describing the decision to purchase home insurance</figcaption>
|                    | Fire                 | No fire        |
|--------------------|----------------------|----------------|
| Take out insurance | No house and $100,00 | House and $0   |
| No insurance       | No house and $100    | House and $100 |
</figure>

Each row (after the first) represents a different action and each column (after the first) represents a different possible state of the world. Their intersections---the four cells that are the combination of an act and a world state---are called outcomes.

# Sets of settings

In [decision theory](https://en.wikipedia.org/wiki/Decision_theory) (and [social choice theory](https://en.wikipedia.org/wiki/Social_choice_theory), [game theory](https://en.wikipedia.org/wiki/Game_theory), [mechanism design](https://en.wikipedia.org/wiki/Mechanism_design), etc.), when presented with a decision, it's often useful to start by taking stock of what information we have available and what information we would like but don't have. Depending on the result of this assessment, we will have better or worse strategies available. That is, we'd like to determine the best strategy or solution given the information available. Are there better strategies that we could execute with more information? What is that information? For example, we approach the question of "Should we buy home insurance?" very differently if we know the precise chance of our house catching on fire. Without key information like that, we have to resort to second-best strategies.

The decision matrix we depicted above reflects one of the [simplest possible]{.noted}[^simpler] settings. In particular, we don't have any probabilities associated with the different states of the world ("Fire" or "No fire") which makes it a "decision under ignorance". Another key limitation is that we don't have a number representing how good or bad each outcome is---our outcomes have not been assigned [cardinal utility](https://en.wikipedia.org/wiki/Cardinal_utility). Instead, our outcomes are only on an [ordinal scale](https://en.wikipedia.org/wiki/Level_of_measurement).

Because this setting is so minimal, it both has wide applicability---it makes very few assumptions that can be contradicted by facts on the ground---and limited insight---the best you can do with minimal information still isn't very good.

<!--more-->

# Dominance

## Prose

One of the rules that exemplifies both broad applicability and limited insight is the dominance rule. Action A weakly dominates action B if it produces an outcome which is at least as good as that of action B in every state of the world. Action A strongly dominates action B if it produces an outcome which is at least as good as that of action B in every state of the world AND is strictly better than action B in at least one state of the world.

## Example

You have the choice of two alternative routes to work. In good conditions, both take 10 minutes. But the second route is prone to traffic and on bad days takes 20 minutes while the first route still takes 10 minutes. With a scenario like this, the dominance rule demands that you take the first route since it is never worse and sometimes better.

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| **Route 1** | 10 minutes       | 10 minutes      |
| Route 2     | 20 minutes       | 10 minutes      |
</figure>

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis (Note that "better than" in this case means later in [ASCIIbetical order](https://en.wikipedia.org/wiki/ASCII#Character_order)---later letters are better than earlier letters). The analysis will update whenever you stop editing text and defocus the text area.

```{=html}
<textarea id="dominance-table" class="decision-table">
|          | State 1 | State 2 |
|----------|---------|---------|
| Action 1 | c       | d       |
| Action 2 | e       | f       |
</textarea>
```

Weak:

::: {#weak-dominance-analysis}
- Action 2 weakly dominates Action 1
:::

Strong:

::: {#strong-dominance-analysis}
- Action 2 strongly dominates Action 1
:::

## Source code

Another way to explain dominance is with source code:

```haskell
dominatesWeakly :: forall cell. PartialOrd cell => PairOfRows cell -> Boolean
dominatesWeakly rows = Foldable.and (NonEmpty.zipWith (>=) row1 row2)
  where
    Tuple row1 row2 = unzipNeMultiSet rows

dominatesStrongly :: forall cell. PartialOrd cell => PairOfRows cell -> Boolean
dominatesStrongly rows =
  Foldable.and (NonEmpty.zipWith (>=) row1 row2) &&
  Foldable.or (NonEmpty.zipWith (>) row1 row2)
  where
    Tuple row1 row2 = unzipNeMultiSet rows
```

To hammer home the point about dominance being applicable in very general settings: `cell` is totally polymorphic except for the constraint that `cell`s form a [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set).

## Math

And the final view on dominance will be [the fully mathematical one]{.noted}[^math]:

### Weak dominance

We can also describe weak dominance $\preccurlyeq_{WD}$ in symbols:

$$a_i \preccurlyeq_{WD} a_j \leftrightarrow \forall s \in S\ (v(a_i, s) \leq v(a_j, s))$$

where $a_i$ and $a_j$ represent the ith and jth actions, $s$ is a particular state of the world from the set $S$ of all states, and $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set) of values $V$.

### Strong dominance

In symbols, strong dominance $\prec_{SD}$ is:

$$a_i \prec_{SD} a_j \leftrightarrow \forall s \in S\ (v(a_i, s) \leq v(a_j, s)) \wedge \exists s \in S\ (v(a_i, s) < v(a_j, s))$$

where $a_i$ and $a_j$ represent the ith and jth actions, $s$ is a particular state of the world from the set $S$ of all states, and $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set) of values $V$.

<hr class="references">

[^simpler]: Ways we could go simpler: only one action, only one state of the world. But these settings are <em>too</em> simple and rather boring.
[^math]: This perspective doesn't really appear in [@peterson2017introduction]; any errors here are my own.
