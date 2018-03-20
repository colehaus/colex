---
title: Income-based utility across countries
tags: social welfare, economics, inequality
published: 2018-03-12
js: dist-countries
---

[Last time](../utilitarian-egalitarianism/), we suggested that we could preserve a preference for egalitarian outcomes without having egalitarianism as a [terminal value](https://en.wikipedia.org/wiki/Instrumental_and_intrinsic_value). Instead, we found that pure utilitarianism can arrive at the same destination via [diminishing marginal utility]( https://en.wikipedia.org/wiki/Marginal_utility#Diminishing_marginal_utility).

But that was all ~~idle wankery~~ pure theory. We can give further fuel to our intuition and actually make contact with reality by applying this line of thought to empirical data. Our two essential inputs were the income distribution and the function describing the diminution of marginal utility with increasing income. [To the literature!](https://www.youtube.com/watch?v=pLMNxVDwUu8).

<!--more-->

# Income distribution by country

After a fairly brief search, I turned up [@wiid]. This dataset has pretty good coverage across countries (as opposed to, for example, only [OECD data](http://www.oecd.org/social/income-distribution-database.htm)).

Let's take a first look at the data:

<figure>
<figcaption>Income distribution by country</figcaption>
<div id="income-by-country"></div>
</figure>

Mostly what you'd expect. Perhaps worth noting that the plots are convex even after being plotted on a log scale.

<hr class="references">
