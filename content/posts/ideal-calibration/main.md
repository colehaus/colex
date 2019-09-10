---
title: Ideal theory as calibration
published: 2018-06-20
tags: ideal theory, political philosophy
series: The Tyranny of the Ideal
css: ideal-calibration
js: ideal-calibration
include-toc: true
---

::: macros
\newcommand{\argmax}{\mathop{\mathrm{argmax}}\limits}
\newcommand{\argmin}{\mathop{\mathrm{argmin}}\limits}
:::

# Intro

[Last time](/posts/utopia-infinitude-secretaries/), I described how [@gaus2016] juxtaposes unidimensional and multidimensional models of justice. I went on to contest the claim that the ideal is otiose in the unidimensional model and made an analogy to [the secretary problem](https://en.wikipedia.org/wiki/Secretary_problem).

This time I'll make the (related) argument directly that there are two distinct uses of ideal theorizing and only one is bad from the unidimensional perspective.

# Dimensionality of justice

Let's try to formalize 'unidimensional' and 'multidimensional' models of justice so we can be sure we're thinking of the same thing. Gaus suggests (and I'll accept) that a key part of any theory of ideal justice includes a function [$e \colon \mathbb{W} \to \mathbb{R}$]{.noted}[^double], a set of possible worlds $\mathbb{W}$. In other words, each such theory should be able to assign a 'justice score' to every possible world. In terms of this machinery, the unidimensional model simply limits itself to using *only* $e$ and $\mathbb{W}$. The multidimensional model on the other hand also gives us a tool to inspect the structure of $\mathbb{W}$ in the form of a [metric](https://en.wikipedia.org/wiki/Metric_(mathematics)) $d \colon \mathbb{W} \times \mathbb{W} \to [0,\inf)$. In other words, the multidimensional model lets us determine how similar two possible worlds are in some way that's not directly related to their justice scores.

To actually use this in a model, we'll also need a way of finding worlds $W$ from $\mathbb{W}$ to evaluate. We denote a a random, 'nearby' (i.e. one with a small distance $d(W, W_c)$ from the then-current world $W_c$) world as $W_r$.

# Ideal as destination

[@gaus2016] and the rest of the literature suggest that the an ideal is useful as a destination. According to Rawls, "By showing how the social world may realize the features of a realistic Utopia, political philosophy provides a long-term goal of political endeavor, and in working toward it gives meaning to what we can do today." [@rawls1993]

In terms of our model, the ideal is $\argmax_{W \in \mathbb{W}} e(W) = W_i$, the possible world that achieves the highest justice score. A wholly naive algorithm would then:

1. Use our ideal $W_i$ to orient societal progress by always picking $\argmin_{W \in \{W_c, W_r\}} d(W, W_i) = W_{bk}$. In other words, on every 'step', someone proposes some random alternative world $W_r$ and this naive algorithm compares it to the current world and picks whichever is closer to the ideal.
2. Stop when $d(W_c, W_i) = 0$.

With this interpretation, it's clear that the ideal as destination as not only otiose in the unidimensional model but nonsensical. The unidimensional perspective was defined by its omission of the metric $d$ so we certainly can't use it in our algorithm to find $W_{bk}$.

<!--more-->

# Ideal as calibration

But, as we suggested in the previous post, this is not the only role that an ideal can play. In real life, we are not so perfectly informed as the simple mathematical model suggests. We can model our ignorance as some combination of:

- Knowledge only of some proper subset of possible worlds: $\mathbb{W}_k \subset \mathbb{W}$
- An inability to evaluate the justice of some alternative worlds. That is, our function is partial: $e_p \colon \mathbb{W} ⇸ \mathbb{R}$

The best world we know of is then $\argmax_{W \in \mathbb{W}_k} e_p(W) = W_{bk}$. Also, there is some cost to exploring possible worlds---if nothing else, the opportunity cost of living in worse worlds.

## Unidimensional

With this understanding in place, we can construct a naive algorithm just like the earlier one we described for the unidimensional case.

1. Use our ideal $W_i$ to orient societal progress by always picking $\argmax{W \in \{W_c, W_r\}} e(W) = W_{bk}$.
2. Stop when $e(W_c) = e(W_i)$.

We see that knowledge of the ideal is crucial in our stopping condition. Without it, it would be hard to determine when to stop the search and stop incurring search costs.

But we could still make a best guess about whether to stay or go, even without knowledge of the ideal. We can treat it as a problem of [statistical inference](https://en.wikipedia.org/wiki/Statistical_inference). We have a sample of possible worlds (the worlds we've experienced) and would like to estimate the true distribution of possible worlds. Once we have an estimate as to the distribution of possible worlds, we can make a more informed decision on whether to continue striving or to accept the status quo.

For example, if we've experienced many mediocre worlds in the past and only a few that are nearly as good as $W_{bk}$ []{.spark #in-upper-spark} (and if we, for the sake of expository convenience, suppose that possible worlds are normally distributed with respect to justice score), we should suspect that we're in the upper tail of the distribution and be correspondingly cautious. Contrariwise, if we've experienced a few bad worlds in the past and many worlds about as good as ours []{.spark #in-middle-spark}, we should suspect that we're in the fattest part of the distribution and that there's still substantial room for improvement.

So even though the ideal isn't absolutely necessary to provide a stopping condition in the unidimensional case, it is useful. Extrema have an outsize influence on our estimate of the distribution of possible worlds. If we've only experienced a handful of mediocre worlds []{.spark #mediocre-spark}, we might suppose that distribution of possible worlds is mediocre. If however, we learn of an ideal world that is far, far better []{.spark #ideal-spark}, our estimate of the distribution changes and so our actions should change accordingly.

## Multidimensional

We can even transport this calibration logic back to the multidimensional perspective. The most interesting task in this setting is demonstrating that ideal as calibration is distinct from ideal as destination: First, suppose we lack an ideal theory and currently occupy a local maximum. If we're sufficiently uncertain about the underlying distribution of possible worlds, our social engineering algorithm may well suggest we abandon this peak in search of higher peaks. We may even eventually end up at the true ideal (global maximum). Now, imagine that we *do* have an ideal theory, but it says that the ideal is only the teensiest smidge better than our current local maximum. Because traversing the landscape of possible worlds is costly, our algorithm almost certainly suggests that we stay at our current peak indefinitely. In other words, knowledge of the ideal has *discouraged* us from pursuing it. It has dictated that we stay where we are rather than pursue the ideal. This is impossible to explain if the ideal *only* has value as a destination.

# Conclusion

The ideal can indeed serve as destination (i.e. we continually move toward the ideal). But it can also serve as calibration. Once we acknowledge our ignorance of possible worlds, we must treat the task of social engineering as a problem of statistical inference. From the statistical inference perspective, the ideal (maximum) is very informative about the underlying distribution of possible worlds and helps us make more informed trade-offs.

<hr class="references">

[^double]: I think Gaus actually jumps to the conclusion of a cardinal evaluation function (rather than ordinal) too quickly, but we'll set that aside for the moment.
