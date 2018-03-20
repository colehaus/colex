---
title: A visual intuition for the instrumental argument for equality
tags: social welfare, economics, inequality
published: 2018-03-03
js: util-egal, draw
css: draw
---

# Introduction

## Equality

[Our moral intuition]{.noted} [^intuition] easily suggests that equality has moral value. The world in which [Robinson Crusoe and Friday](https://en.wikipedia.org/wiki/Robinson_Crusoe_economy) share coconuts seems vastly better than the world in which Crusoe sits on his coconut throne and doles out the barest sustenance to Friday. It is easy to leap from this intuition to the conclusion that equality is an [intrinsic](https://en.wikipedia.org/wiki/Instrumental_and_intrinsic_value) good. But [let's not posit plurality without necessity](https://en.wikipedia.org/wiki/Occam%27s_razor). In this post, let's make a stop at the proposition that equality is instead only an instrumental good.

## Equality instrumentally

If our sole intrinsic good is something like [utils](https://en.wikipedia.org/wiki/Utility), we will still often prefer equality to inequality. What sorcery could introduce such deontological concepts into our consequentialist paradise? The incantation is ["diminishing marginal utility"]( https://en.wikipedia.org/wiki/Marginal_utility#Diminishing_marginal_utility). It is an empirical proposition (and mostly fact) that humans do not enjoy the 10th print copy of [_Harrison Bergeron_](https://en.wikipedia.org/wiki/Harrison_Bergeron) as much as the first. Thus, a _Harrison_ hoarder can improve overall well-being by sharing the love. Losing copy 10 hurts less than gaining copy 1 helps.

# Visualizing the utilitarian consequences of income distribution

We can start to build a visual intuition for this instrumental egalitarianism. Suppose we have an income distribution that looks like this:

<form>
<fieldset class="draw">
<figure>
<figcaption>Income in dollars by percentile for hypothetical population</figcaption>
<div id="income-distribution"></div>
</figure>
<button type="button">Clear</button>
</fieldset>
</form>

And suppose that the marginal utility of the last dollar of income looks like this:

<form>
<fieldset class="draw">
<figure>
<figcaption>Hypothetical marginal utility of last dollar over a range of incomes</figcaption>
<div id="marginal-utility"></div>
</figure>
<button type="button">Clear</button>
</fieldset>
</form>

Then, via some math (check [the preceding post](../utilitarian-egalitarianism-notebook/) for details), we can figure out the utility across the population:

<figure>
<figcaption>Utility by percentile for hypothetical populations based on income distribution and marginal utility of money.</figcaption>
<div id="utility-distribution"></div>
</figure>

In addition to showing the utility distribution for the income distribution specified in the first chart, we also show the utility distribution of a hypothetical perfectly egalitarian population (with the same total income). Finally, the dashed lines show the mean utility in each population.

The default functions plotted in the first two charts (income distribution and marginal utility of last dollar) are merely suggestive and the true fact of the matter is empirically determined. If you don't like the suggestions, feel free to draw in your own functions (click and drag). The final chart showing the population utility distribution will update accordingly.

Some suggestions:

- See what happens when you draw in a more aggressively inegalitarian income distribution
- See what happens when you draw in an egalitarian income distribution
- See what happens when you draw in constant marginal returns
- See what happens when you draw in increasing marginal returns

# Conclusions

For any given level of total income, diminishing marginal returns imply that an egalitarian distribution of that income produces greater average and total utility. This is reflected visually by the dashed mean for the inegalitarian utility distribution being below the dashed mean for the egalitarian utility distribution.

# Caveats

The presentation here is highly simplified in a way that's probably misleading. Some complications:

1. Income isn't the only source of utility. To the extent that other sources of utility are uncorrelated with income, acknowledging other contributors to utility would tend to reduce the relative importance of inequality. For example, if income accounted for only 10% of people's utility, even radical inequality would mean only a relatively minor missed opportunity.
2. Diminishing marginal utility isn't the only route by which inequality may affect utility. People may have direct preferences about the income distribution (i.e. envy and resentment). The impact of this on our analysis depends on the nature of these preferences. If people's preferences are sensitive to absolute differences in terms of dollars, our story so far understates the importance of equality. If their preferences are sensitive only to their rank in the overall distribution, most egalitarian improvements are irrelevant so this consideration can be omitted.

[^intuition]: Obviously, this is a bit handwavy because people have different moral intuitions. But many of our moral intuitions are widely shared. That includes intuitions about reciprocity [@haidt04].

<hr class="references">
