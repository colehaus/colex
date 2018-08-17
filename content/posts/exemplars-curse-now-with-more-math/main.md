---
title: Exemplar's curse—Now with 80% more math!
published: 2018-07-06
tags: decision theory, interactive
js: exemplars-curse
css: exemplars-curse
---

# Intro

[Last time](/posts/exemplars-curse-singapore/), I outlined the exemplar's curse in the context of Singapore with a parable and an informal description. Rest easy; I've heard your needful clamoring---I'll now describe the curse more precisely with math.

# The exemplar's curse

Restating the core idea in words: The exemplar's curse occurs when we select an exemplar from a set of outcomes which resulted from both stochastic and deterministic factors. If many outcomes have similarly compelling deterministic factors, the chosen winner is probably unusually lucky. Reversion to the mean then suggests that the chosen winner will disappoint when the deterministic factors are replicated.

## Model

(If you don't like the mathematical exposition that follows, you can also try [@barnett2004].)

We can model this with the use of [random variables](https://en.wikipedia.org/wiki/Random_variable). We'll call our bundle of deterministic factors $\mathcal{D}$ and say they range uniformly in cumulative value from $0$ to $D$ where $D$ is finite. Our bundle of stochastic factors $\mathcal{S}$ range uniformly in value from from $0$ to $S$ where $S$ is finite. Since we observe only visible outcomes rather than underlying causal factors, we see $\mathcal{O} = \mathcal{D} + \mathcal{S}$. The exemplar's curse is then about the inferrable properties of the causal factors $\mathcal{D}$ corresponding to the selected maximum $\mathcal{O}$ from a set of outcomes $\mathbb{O}$. In other words, if we have a set of observable outcomes $\mathbb{O}$ and select the best outcome from that set $\mathcal{O}$, what can we infer about the underlying structure of $\mathcal{O}$---how big are that $\mathcal{O}$'s $\mathcal{D}$ and $\mathcal{S}$?

# False exemplars

In the last post, we only went so far as to claim that we should expect replicating causal factors to produce disappointing outcomes due to reversion to the mean. That is, the stochastic factors $\mathcal{S}_1$ for the maximum outcome $\mathcal{O}_1 = \max \mathbb{O}$ are likely better than average ($\frac{S}{2}$). If we generate a new outcome $\mathcal{O}_2$ using the same deterministic factors $\mathcal{D}$ that served us well in $\mathcal{O}_1$, we should expect our new stochastic factors to be worse $\mathcal{S}_2 < \mathcal{S}_1$ and so we should expect $\mathcal{O}_2 < \mathcal{O}_1$.

This leaves a open a compelling retort. One could say, "Even though I'm too optimistic about the eventual outcome, in selecting the exemplar, I'm still selecting the best deterministic factors. That means I'm still making the best choice I can so no harm done."

Alas, this is not true. Depending on the parameters, there could be only a vanishingly small chance that the bundle of deterministic factors corresponding to the best outcome (the sum of the deterministic and stochastic factors) is also the best bundle of deterministic factors when looking *only* at the deterministic factors. In symbols, supposing we have a projection function $p_\mathcal{D} : \mathbb{O} \rightarrow \mathbb{D}$ which finds the bundle of deterministic factors $\mathcal{D}$ used in outcome $\mathcal{O}$, we're interested in $P(\max \mathbb{D} = p_\mathcal{D}(\max \mathbb{O}))$.

For example, if we choose the max from 1000 outcomes and the value of stochastic factors ranges from 0 to 100 while the value of deterministic factors ranges from 0 to 1, we should be quite surprised if our best outcome actually has the best deterministic factors.

# Calculator

We can help build up an intuition around this math using the calculator below. The calculator uses [Monte Carlo methods](https://en.wikipedia.org/wiki/Monte_Carlo_method) to estimate the probability that the maximum outcome corresponds to the maximum bundle of deterministic factors.

<!--more-->

You can change the range of values for stochastic and deterministic factors and change the number of outcomes we're taking the max from (i.e. Our winner is the winner out of how many contestants?).

This results in:

 - The mean value of stochastic factors in best outcomes across Monte Carlo samples. This allows us to estimate the magnitude of expected mean regression.
 - An estimated probability that our observed best outcome actually corresponds to the best bundle of deterministic factors.
 - A chart showing all the trials used to produce the estimate. One interesting thing to note about the chart is that the number of points on the diagonal corresponds to the probability of 'success' (i.e. the probability that the best outcome also has the best deterministic factors).

<form>
<p>Stochastic factors: 0 to <input id="stochastic-max" type="number" min="0" value="1" step="0.1" /></p>
<p>Deterministic factors: 0 to <input id="deterministic-max" type="number" min="0" value="1" step="0.1" /></p>
<p>Number of outcomes to choose max from: <input id="num-contestants" type="number" min="1" value="5" /></p>
</form>
<output>
<p>Mean of stochastic factors across samples: <span id="mean-stochastic">0.75</span></p>
<p>Probability best measured outcome has best deterministic factors: <span id="prob-max">0.5</span></p>
<figure>
<figcaption>Comparing deterministic component of best outcome to best deterministic component across Monte Carlo samples</figcaption>
<div id="combined-chart"></div>
</figure>
</output>

Some trends worth noting:

- As the stochastic factors get relatively larger, the probability goes down
- As the deterministic factors get relatively larger, the probability goes down
- As the number of outcomes goes up, the probability goes down and the mean of stochastic factors goes up

This confirms the intuition highlighted by the example a few paragraphs up.

# Conclusion

So we see that the exemplar's curse is even worse than we thought. It's not just about our outcomes regressing to the mean in spite of making the best possible choice. It will often lead us to pick the wrong exemplar!

<hr class="references">
