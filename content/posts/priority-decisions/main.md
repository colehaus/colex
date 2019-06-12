---
title: Priority decisions
series: An Introduction to Decision Theory
published: 2019-01-30
edited: 2019-06-10
tags: decision theory, interactive, yaas
js: decision-demos
css: decision-demos
---

[Last time](/posts/dominated-decisions/), we introduced the basic setup of decision theory and examined the dominance decision rule. We also emphasized that the dominance decision rule is "weak" because it applies in very general settings with limited information to go on.

This time, we'll look at other decision rules that apply in that very general setting. They're still decisions under ignorance---no probabilities associated with states of the world---and outcomes are still measured only on an [ordinal scale](https://en.wikipedia.org/wiki/Level_of_measurement).

# Maximin

The first such decision rule is [maximin](https://en.wikipedia.org/wiki/Minimax).

## Prose

Maximin suggests that in any decision scenario, we look to the worst outcome that may come to pass under each plan of action. We should then pick the action which has the best such outcome. That is, we pick the action with the best worst case---maximize our minimum.

## Example

You have the choice of two alternative routes to work. In good conditions, the first route takes 10 minutes and the second route 5 minutes. But the second route is prone to traffic and on bad days takes 20 minutes while the first route still takes 10 minutes. 

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Low traffic day |
|:------------|:-----------------|:----------------|
| **Route 1** | 10 minutes       | 10 minutes      |
| Route 2     | 20 minutes       | 5 minutes       |
</figure>

With a scenario like this, the maximin rule demands that you take the first route since its worst case is only 10 minutes while the second route's worst case is 20 minutes.

<!--more-->

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis (Note that "better than" in this case means later in [ASCIIbetical order](https://en.wikipedia.org/wiki/ASCII#Character_order)---later letters are better than earlier letters). The analysis will update whenever you stop editing text and defocus the text area.

```{=html}
<textarea id="maximin-table" class="decision-table">

|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | a       | y       | z      |
| Action 2 | b       | c       | y      |
| Action 3 | b       | d       | z      |
</textarea>
```

::: {#maximin-analysis}
- Action 2 beats Action 1
- Action 2 beats Action 3
- Action 3 beats Action 1
- Action 3 beats Action 2
:::

We see that with the default input table, Actions 2 and 3 beat Action 1 since the worst case of 'b' is preferable to the worst case of 'a'. Actions 2 and 3 beat each other since they have the same worst case---'b'---and we're using a weak maximin here.

## Code

We can also explain maximin with its implementing source code:

```haskell
maximin :: forall cell. Ord cell => PairOfRows cell -> Boolean
maximin rows = Foldable1.minimum row1 Prelude.>= Foldable1.minimum row2
  where
    Tuple row1 row2 = unzipNeMultiSet rows
```

Again, maximin applies in very general settings because the only constraint we must satisfy is that `cell`s are [[orderable](https://en.wikipedia.org/wiki/Total_order)]{.noted}[^order].

## Math

We can also describe maximin $\preccurlyeq_{MaMi}$ in symbols:

$$a_i \preccurlyeq_{MaMi} a_j \leftrightarrow \min_{s \in S} v(a_i, s) \leq \min_{s \in S} v(a_j, s)$$

where $a_i$ and $a_j$ represent the ith and jth actions, $s$ is a particular state of the world from the set $S$ of all states, and $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the [total order](https://en.wikipedia.org/wiki/Total_order) order of values $V$.

# Maximax

The second decision rule we'll look it maximax. It's considerably more optimistic than maximin.

## Prose

Maximax suggests that in any decision scenario, we look to the best outcome that may come to pass under each plan of action. We should then pick the action which has the best such outcome. That is, we pick the action with the best best case---maximize our maximum.

## Example

Suppose you're choosing between routes to work again:

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Low traffic day |
|:------------|:-----------------|:----------------|
| Route 1     | 10 minutes       | 10 minutes      |
| **Route 2** | 20 minutes       | 5 minutes       |
</figure>

With a scenario like this, the maximax rule demands that you take the second route since its best case is only 5 minutes while the first route's best case is 10 minutes.

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis (Note that "better than" in this case means later in [ASCIIbetical order](https://en.wikipedia.org/wiki/ASCII#Character_order)---later letters are better than earlier letters). The analysis will update whenever you stop editing text and defocus the text area.


```{=html}
<textarea id="maximax-table" class="decision-table">
|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | a       | y       | z      |
| Action 2 | b       | c       | y      |
| Action 3 | b       | d       | z      |
</textarea>
```

::: {#maximax-analysis}
- Action 1 beats Action 2
- Action 1 beats Action 3
- Action 3 beats Action 1
- Action 3 beats Action 2
:::

We see that with the default input table, Actions 1 and 3 beat Action 2 since the best case of 'z' is preferable to the best case of 'y'. Actions 1 and 3 beat each other since they have the same best case---'z'---and we're using a weak maximax here.

## Code

We can also explain maximax with its implementing source code:

```haskell
maximax :: forall cell. Ord cell => PairOfRows cell -> Boolean
maximax rows = Foldable1.maximum row1 Prelude.>= Foldable1.maximum row2
  where
    Tuple row1 row2 = unzipNeMultiSet rows
```

Again, maximax applies in very general settings because the only constraint we must satisfy is that `cell`s are [orderable](https://en.wikipedia.org/wiki/Total_order).

## Math

We can also describe maximax $\preccurlyeq_{MaMa}$ in symbols:

$$a_i \preccurlyeq_{MaMa} a_j \leftrightarrow \max_{s \in S} v(a_i, s) \leq \max_{s \in S} v(a_j, s)$$

where $a_i$ and $a_j$ represent the ith and jth actions, $s$ is a particular state of the world from the set $S$ of all states, and $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the [total order](https://en.wikipedia.org/wiki/Total_order) order of values $V$.

# Leximin

The final decision rule we'll look at today is leximin.

## Prose

We can think of leximin as maximin with a tiebreaking procedure. In leximin, if the worst outcomes are equal, we then look to the second worst outcomes and prefer whichever action has the better second worst outcome. This tiebreaking procedure continues all the way up the ordered list of worst-to-best outcomes per action and only declares indifference between the actions if outcomes are the same at each step.

Alternatively, we can think of leximin as the general procedure and maximin and maximax as special cases of it. Leximin looks at the whole sorted list of outcomes for each action while maximin and maximax each only look at one end of the list.

## Example

Suppose you're choosing between routes to work again:

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Medium traffic day | Low traffic day |
|:------------|:-----------------|:-------------------|:----------------|
| Route 1     | 20 minutes       | 12 minutes         | 4 minutes       |
| **Route 2** | 20 minutes       | 8 minutes          | 8 minutes       |
</figure>

Route 2 is preferable because the two routes tie in the worst case---20 minutes--- but route 2's 8 minutes is a better a second worst case than route 1's 12 minutes.

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis (Note that "better than" in this case means later in [ASCIIbetical order](https://en.wikipedia.org/wiki/ASCII#Character_order)---later letters are better than earlier letters). The analysis will update whenever you stop editing text and defocus the text area.

```{=html}
<textarea id="leximin-table" class="decision-table">
|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | a       | y       | z      |
| Action 2 | b       | c       | y      |
| Action 3 | b       | d       | z      |
</textarea>
```

::: {#leximin-analysis}
- Action 2 beats Action 1
- Action 3 beats Action 1
- Action 3 beats Action 2
:::

We see that with the default input table, Actions 2 and 3 beat Action 1 since the worst case of 'b' is preferable to the worse case of 'a'. Actions 3 beats Action 2 since the second worst case of 'd' is preferable to the second worst case 'c'

## Code

We can also explain leximin with its implementing source code:

```haskell
leximin :: forall cell. Ord cell => PairOfRows cell -> Boolean
leximin rows =
    fromMaybe true <<< List.head <<< NonEmpty.mapMaybe keepNonEq $
    NonEmpty.zipWith compare (NonEmpty.sort row1) (NonEmpty.sort row2)
  where
    Tuple row1 row2 = unzipNeMultiSet rows
    keepNonEq GT = Just true
    keepNonEq LT = Just false
    keepNonEq EQ = Nothing
```

Again, leximin applies in very general settings because the only constraint we must satisfy is that `cell`s are [orderable](https://en.wikipedia.org/wiki/Total_order).

[^order]: The only change in setting/assumptions we've made between dominance and maximin is upgrading our `cell` valuations from a partial order to a total order. This isn't strictly necessary but incomparability becomes much more cumbersome with maximin because more pairwise comparisons are required. With dominance, the only comparisons needed to determine one action superior to another are those across two outcomes in each particular state of the world. With maximin, we need to be able to compare all the outcomes for a given state against each other and then compare the worst of these across actions.
