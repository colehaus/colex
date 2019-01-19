---
title: Dominated decisions
series: An Introduction to Decision Theory
published: 2019-01-17
tags: decision theory, interactive, yaas
js: dominated-decisions
css: dominated-decisions
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

In [decision theory](https://en.wikipedia.org/wiki/Decision_theory) (and [social choice theory](https://en.wikipedia.org/wiki/Social_choice_theory), [game theory](https://en.wikipedia.org/wiki/Game_theory), [mechanism design](https://en.wikipedia.org/wiki/Mechanism_design), etc.), when presented with a decision, it's often useful to start by taking stock of what information we have available and what information we would like but don't have. Depending on the result of this assessment, we will have better or worse strategies available. That is, we'd like to determine the best strategy or solution given the information available. And are there better strategies that we could execute with more information? What is that information? For example, we approach the question of "Should we buy home insurance?" very differently if we know the precise chance of our house catching on fire. Without key information like that, we have to resort to second-best strategies.

# Dominance

The decision matrix we depicted above reflects one of the [simplest possible]{.noted}[^simpler] settings. In particular, we don't have any probabilities associated with the different states of the world ("Fire" or "No fire") which makes it a "decision under ignorance". Another key limitation is that we don't have a number representing how good or bad each outcome is---that is, our outcomes have not been assigned [cardinal utility](https://en.wikipedia.org/wiki/Cardinal_utility).

Because this setting is so minimal, it both has wide applicability---it makes very few assumptions that can be contradicted by facts on the ground---and limited insight---the best you can do with minimal information still isn't very good.

## Prose

One of the rules that exemplifies both broad applicability and limited insight is the [dominating decision rule](https://en.wikipedia.org/wiki/Dominating_decision_rule). Action A weakly dominates action B if it produces an outcome which is at least as good as that of action B in every state of the world. Action A strongly dominates action B if it produces an outcome which is at least as good as that of action B in every state of the world AND is strictly better than action B in at least one state of the world.

<!--more-->

## Interactive

If the English description isn't that helpful try poking around with this interactive analysis (Note that "better than" in this case means later in [ASCIIbetical order](https://en.wikipedia.org/wiki/ASCII#Character_order)). The analysis will update whenever you stop editing text and defocus the text area.

<textarea id="decision-table">
```{=html}

|          | State 1 | State 2 |
|----------|---------|---------|
| Action 1 | c       | d       |
| Action 2 | e       | f       |
```
</textarea>

<div id="decision-analysis">
- Action 2 weakly dominates Action 1

- Action 2 strongly dominates Action 1
</div>

## Source code

Yet another way to explain dominance is with source code:

```haskell
dominatesWeakly ::
  forall rowId columnId cell column.
  Eq rowId => PartialOrd cell =>
  Table rowId columnId cell (NonEmptyList cell) column -> rowId -> rowId -> Maybe Boolean
dominatesWeakly table rowId1 rowId2 =
  case Tuple (Table.row table rowId1) (Table.row table rowId2) of
    Tuple (Just row1) (Just row2) ->
      Just $ Foldable.all ((==) true) (NonEmpty.zipWith (>=) row1 row2)
    Tuple _ _ -> Nothing

dominatesStrongly ::
  forall rowId columnId cell column.
  Eq rowId => PartialOrd cell =>
  Table rowId columnId cell (NonEmptyList cell) column -> rowId -> rowId -> Maybe Boolean
dominatesStrongly table rowId1 rowId2 =
  case Tuple (Table.row table rowId1) (Table.row table rowId2) of
    Tuple (Just row1) (Just row2) ->
      Just $
        Foldable.all ((==) true) (NonEmpty.zipWith (>=) row1 row2) &&
        Foldable.any ((==) true) (NonEmpty.zipWith (>) row1 row2)
    Tuple _ _ -> Nothing
```

To hammer home the point about dominance being applicable in very general settings: `rowId`, `columnId` and `cell` are totally polymorphic types except for the constraint that `rowId`s can be compared for equality and `cell`s form a [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set).

<hr class="references">

[^simpler]: Ways we could go simpler: only one action, only one state of the world. But these settings are <em>too</em> simple and rather boring.
