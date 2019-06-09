---
title: Ignorant miscellany
series: An Introduction to Decision Theory
published: 2019-05-30
tags: decision theory, interactive, yaas
js: decision-demos
css: decision-demos
---

[Last time](/posts/priority-decisions/), we [continued](/posts/dominated-decisions/) our discussion of decision rules that apply in information-poor settings. In particular, we've been focusing on "decisions under ignorance"---decisions where the probabilities associated with various states of the world are unknown.

This time, we'll look at three final decision rules in this category.

# Optimism-pessimism

The first decision rule we'll look at is the optimism-pessimism [TODO] rule. 

## Prose

Conceptually, optimism-pessimism is a generalization of the [maximin](/posts/priority-decisions/#maximin) and [maximax](/posts/priority-decisions/#maximax) rules. The maximin rule tells us to make the decision which has the best worst case outcome. The maximax rule tells us to make the decision which has the best best case outcome. 

The optimism-pessimism rule tells us to look at both the best outcome which may come to pass after taking a particular action and the worst outcome which may come to pass. Then we should take a weighted average of the best and worst case outcome for each action and take the action that has the best such average. The weighting used in the decision rule is a parameter that the decision-maker [TODO] is free to choose based on how optimistic or pessimistic they are. So really the optimism-pessimism rule is a family of rules parameterized by a weighting factor.

Also, it's worth noting that the optimism-pessimism family of rules no longer work in the fully general setting we were working with in previous posts. While we still don't have probabilities associated with states of the world, we will need to move from an [ordinal scale](https://en.wikipedia.org/wiki/Level_of_measurement) of outcomes to an [interval scale](https://en.wikipedia.org/wiki/Level_of_measurement). This shift is necessary because it doesn't make sense to take a weighted average of ordinal data.

## Example

You have the choice of two alternative routes to work. In good conditions, the first route takes 10 minutes and the second route 5 minutes. But the second route is prone to traffic and on bad days takes 20 minutes while the first route still takes 10 minutes. 

<figure>
<figcaption>Decision matrix about route to work.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| Route 1     | 10 minutes       | 10 minutes      |
| Route 2     | 20 minutes       | 5 minutes       |
</figure>

If you're perfectly balanced between optimism and pessimism, the optimism-pessimism rule is indifferent between the two routes here. If you're more pessimistic than you are optimistic, you should take route 1--just like in maximin. If you're more optimistic than you are pessimistic, you should take route 2--just like in maximax.

<!--more-->

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis. The analysis will update whenever you stop editing text and defocus the text area or whenever you update the optimism parameter.

(The fact that we've shifted from strings to numbers in the cells is a reflection of our shift from an ordinal scale to an interval scale.)

Optimism: <input id="optimism-pessimism-alpha" min="0" max="1" step="0.1" type="number" value="0.5" />

<textarea id="optimism-pessimism-table" class="decision-table">
```{=html}
|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | 4       | 5       | 6      |
| Action 2 | 3       | 5       | 8      |
| Action 3 | 1       | 5       | 9      |
```
</textarea>

::: {#optimism-pessimism-analysis}
- Action 1 beats Action 3
- Action 2 beats Action 1
- Action 2 beats Action 3
- Action 3 beats Action 1
:::

## Code

We can also explain the optimism-pessimism family of rules with their implementing source code:

```haskell
optimismPessimism ::
  forall n cell.
  Ord cell => Semiring cell => Ring n =>
  (n -> cell) -> Proportion n -> PairOfRows cell -> Boolean
optimismPessimism toCell α rows = value row1 >= value row2
  where
    value row =
      toCell (Proportion.unMk α) * Foldable1.maximum row +
      toCell (one - Proportion.unMk α) * Foldable1.minimum row
    Tuple row1 row2 = unzipNeMultiSet rows
```

`α` is the optimism parameter and `toCell` is a way of harmonizing its type with the `cell` type. Interestingly, we have leaped all the way from requiring `cell` only to be orderable---in maximin and maximax---to requiring that `cell` be a semiring [TODO]---taking a weighted average requires the ability to both multiply and add.

## Math

We can also describe the optimism-pessimism decision rules $\preccucrlyeq_{OptPes}$ in symbols:

$$a_i \preccurlyeq_{OptPes} a_j \leftrightarrow \alpha \cdot \max_{s \in S} v(a_i, s) + (1 - \alpha) \cdot \min_{s \in S} v(a_i, s) \leq \alpha \cdot \max_{s \in S} v(a_j, s) + (1 - \alpha) \cdot \min_{s \in S} v(a_j, s)$$

where $\alpha$ is the weight controlling optimism vs. pessimism, $a_i$ and $a_j$ represent the ith and jth action, $s$ is a particular state of the world from the set $S$ of all states, and $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the interval scale of values $V$.

# Minimax regret

## Prose

Regret is the difference between your actual outcome and the outcome you could have achieved if you had predicted the best possible action. If it took you 10 minutes to get to work and then you find that another route would have taken you only 5 minutes, you have 5 minutes worth of regret.

Minimax regret counsels that you take the action which minimizes the amount of regret you have in the least favorable state of the world---minimize your maximum regret.

Again we lose a bit of generality as our outcomes must be measured on an interval scale (to support computing regrets) rather than an ordinal scale.

The other point worth making is that we have lost a property known as "independence of irrelevant alternatives" [TODO]. Suppose we are making a decision and only have actions A and B available. Furthermore, suppose minimax regret says that the best action is A. If we add a third action C that is worse than both A and B (in minimax regret terms), minimax regret may now insist that action B is best. This is pretty weird! We'll look at an example below.

## Example

Suppose you're choosing between routes to work again:

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| **Route 1** | 20 minutes       | 20 minutes      |
| Route 2     | 30 minutes       | 15 minutes      |
</figure>

With a scenario like this, minimax regret demands that you take the first route. To see why, we'll first transform the table into a table of regrets:

<figure>
<figcaption>Table of regrets corresponding to table immediately above.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| **Route 1** | 0 minutes        | 5 minutes       |
| Route 2     | 10 minutes       | 0 minutes       |
</figure>

When we perform minimax on this table, it's clear that the first route is preferable. Our worst case regret is only five minutes while our worst case regret for the second route is 10 minutes.

### Irrelevant alternatives

Suppose that we discover a third possible route to work. If we follow the minimax regret rule, this might cause us to switch from route 1 to route 2:

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| Route 1     | 20 minutes       | 20 minutes      |
| **Route 2** | 30 minutes       | 15 minutes      |
| Route 3     | 40 minutes       | 5 minutes       |
</figure>

<figure>
<figcaption>Table of regrets corresponding to table immediately above.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| Route 1     | 0 minutes        | 15 minutes      |
| **Route 2** | 10 minutes       | 10 minutes      |
| Route 3     | 20 minutes       | 0 minutes       |
</figure>

Because route 3 is even faster than route 2 on low traffic days, it further increases route 1's maximum regret. It also increases route 2's regret on low traffic days but doesn't increase route 2's _maximum_ regret.

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis. The analysis will update whenever you stop editing text and defocus the text area.

<textarea id="minimax-regret-table" class="decision-table">
```{=html}

|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | 4       | 5       | 6      |
| Action 2 | 3       | 5       | 8      |
| Action 3 | 1       | 5       | 9      |
```
</textarea>

Best actions:

::: {#minimax-regret-analysis}
- Action 2
:::

## Code

The code itself is a bit too ugly to be illuminating in this case, but the type signature does have a few things worth pointing out.

```haskell
minimaxRegret ::
  forall rowId columnId cell.
  Hashable cell => Hashable columnId => Hashable rowId =>
  Ord cell => Ring cell =>
  Table rowId columnId cell -> NonEmpty HashSet rowId
```

First, `cell` must now be a ring [TODO] which is a bit stronger than the semiring requirement of optimism-pessimism. This is because computing regrets requires subtraction. 

Second, the decision rule no longer operates on a pair of rows. In all our previous decision rules, we described the decision scenario with `PairOfRows cell`---independence of irrelevant alternatives meant that any context from other actions was irrelevant to the verdict. Here we must take a full `Table` because the verdict `minimaxRegret` returns for two rows may depend on some third row not under active consideration. This is the price we pay for losing independence of irrelevant alternatives.

## Math

We can also describe the minimax regret decision rule $\preccucrlyeq_{Reg}$ in symbols:

$$a_i \preccurlyeq_{Reg} a_j \leftrightarrow \max_{s \in S}(\max_{a \in A} v(a, s) - v(a_i, s)) \geq \max_{s \in S}(\max_{a \in A} v(a, s) - v(a_j, s))$$

where $a_i$ and $a_j$ represent the ith and jth action, $s$ is a particular state of the world from the set $S$ of all states, $a$ is an action from the set $A$ of all actions, $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the interval scale of value $V$. 

We see the entrance of irrelevant alternatives here in that we have a $\max_{a \in A}$ term for the first time. We're no longer looking at $a_i$ and $a_j$ in isolation.

# Indifference

## Prose

The final rule we'll look at is "[the principle of indifference](TODO)", also sometimes called "the principle of insufficient reason".

We've emphasized throughout that we're working in a fairly general setting with limited information. What if we just pretended we weren't? If we had probabilities associated with states of the world, we could just use good old [expected value maximization](TODO) as our decision rule. The principle of indifference says that in the absence of information to the contrary, we should just assign equal probabilities to all states of the world. Then we can proceed with expected value maximization.

Of course, there are problems with explicitly representing our ignorance probabilistically. Which we've in fact already [discussed](/posts/list-problems-ignorant-priors/).

## Example

Suppose you're choosing between routes to work again:

<figure>
<figcaption>Decision matrix about route to work. Preferred action in bold.</figcaption>
|             | High traffic day | Low traffic day |
|-------------|------------------|-----------------|
| **Route 1** | 10 minutes       | 10 minutes      |
| Route 2     | 20 minutes       | 5 minutes       |
</figure>

Because there are only two possible states of the world and we're pretending we have no probabilities associated with these states, the principle of indifference tells us to assign a probability of 1/2 to each state. Once we do, the expected value calculation is straightforward and favors the first route.

<figure>
<figcaption>Decision matrix about route to work after assigning probabilities. Preferred action in bold.</figcaption>
|             | High traffic day; p=0.5 | Low traffic day; p=0.5 | Expected value            |
|-------------|-------------------------|------------------------|---------------------------|
| **Route 1** | 10 minutes              | 10 minutes             | 0.5 * 10 + 0.5 * 10 = 10  |
| Route 2     | 20 minutes              | 5 minutes              | 0.5 * 20 * 0.5 * 5 = 12.5 |
</figure>

## Interactive

If the above description isn't sufficient, try poking around with this interactive analysis. The analysis will update whenever you stop editing text and defocus the text area. (Floating point foolishness [TODO] possible.)

<textarea id="indifference-table" class="decision-table">
```{=html}

|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | 0       | 5       | 6      |
| Action 2 | 3       | 5       | 8      |
| Action 3 | 1       | 5       | 9      |
```
</textarea>

::: {#indifference-analysis}
- Action 2 beats Action 1
- Action 2 beats Action 3
- Action 3 beats Action 1
:::

## Code

```haskell
indifference ::
  forall cell.
  Hashable cell =>
  Ord cell => Semiring cell =>
  (Proportion Number -> cell) -> PairOfRows cell -> Boolean
indifference toCell rows =
  maximizesExpectedUtility toCell <<<
  neMultiSetMap (Tuple prob) $ rows
  where
    prob = unsafeMkHashProp $ 1.0 / Int.toNumber (Foldable.length rows)
```

Note that we're back to only requiring `Semiring` of `cell` and taking pairs of rows at a time instead of a whole table---thanks independence of irrelevant alternatives!

## Math

We can also describe the indifference decision rule $\preccucrlyeq_{Ind}$ in symbols:

$$a_i \preccurlyeq_{Ind} a_j \leftrightarrow \sum_{x=1}^{n} \frac{1}{n} v(a_i, s_x) \leq \sum_{x=1}^{n} \frac{1}{n} v(a_j, s_x)$$

where $a_i$ and $a_j$ represent the ith and jth action, $n$ is the number of states of the world, $s_x$ is a particular state of the world selected by index $x$, and $v : A \times S \to V$ is a function mapping an action in a particular state of the world to an element in the interval scale of values $V$.
