---
title: Uncertainty and sensitivity analysis of GiveWell's top charity rankings
date: 2019-09-29
tag: statistics, development
series: GiveWell cost-effectiveness analysis analysis
---

In the [last](/posts/uncertainty-analysis-of-givewell-cea/) [two](/posts/sensitivity-analysis-of-givewell-cea/) posts, we performed uncertainty and sensitivity analyses on GiveWell's charity cost-effectiveness estimates. Our outputs were, respectively:

- probability distributions describing our uncertainty about the value per dollar obtained for each charity and
- estimates of how sensitive each charity's cost-effectiveness is to each of its input parameters

One problem with this is that we are [not supposed to take the cost-effectiveness estimates literally](https://blog.givewell.org/2011/08/18/why-we-cant-take-expected-value-estimates-literally-even-when-theyre-unbiased/). Arguably, the real purpose of GiveWell's analysis is not to produce exact numbers but to assess the relative quality of each charity evaluated.

Another issue is that by treating each cost-effectiveness estimate as independent we underweight parameters which are shared across many models. For example, the moral weight that ought to be assigned to increasing consumption shows up in many models. If we consider all the charity-specific models together and the relations between their outputs, this input receives a higher sensitivity index.

# Metrics on rankings

We can solve both of these problems by abstract away from particular values in the cost-effectiveness analysis and looking at the overall rankings returned. That is we want to transform:

<figure>
<figcaption>GiveWell's cost-effectiveness estimates for its top charities</figcaption>
| Charity                            | Value per $10,000 donated |
| :----                              |                       --: |
| GiveDirectly                       |                        38 |
| The END Fund                       |                       222 |
| Deworm the World                   |                       738 |
| Schistosomiasis Control Initiative |                       378 |
| Sightsavers                        |                       394 |
| Malaria Consortium                 |                       326 |
| Against Malaria Foundation         |                       247 |
| Helen Keller International         |                       223 |
</figure>

into:

<figure>
<figcaption>Givewell's top charities ranked from most cost-effective to least</figcaption>
- Deworm the World
- Sightsavers
- Schistosomiasis Control Initiative
- Malaria Consortium
- Against Malaria Foundation
- Helen Keller International
- The END Fund
- GiveDirectly
</figure>

But how do we usefully express [probabilities over rankings]{.noted}[^ranking-probs] (rather than probabilities over simple cost-effectivness numbers)? The approach we'll follow below is to characterize a ranking produced by a run of the model by computing its distance from the reference ranking listed here (i.e. GiveWell's current best estimate). Our output probability distribution will then express how far we expect to be from the reference ranking. For example, if the distribution is narrow and near 0, that means our uncertain input parameters mostly produce results similar to the reference ranking. If the distribution is wide and far from 0, that means our uncertain input parameters produce results that are highly uncertain and not necessarily similar to the reference ranking.

## Spearman's footrule

What is this mysterious distance metric between rankings that enables the above approach? One such metric is called Spearman's footrule distance. It's defined as:

::: .skippable
$$d_{fr}(u, v) = \sum_{c \in A} |\text{pos}(u,c) - \text{pos}(v, c)|$$

where:

- $u$ and $v$ are rankings, 
- $c$ varies over all the elements $A$ of the rankings and 
- $\text{pos}(r, x)$ returns the integer position of item $x$ in ranking $r$. 

In other words, the footrule distance between two rankings is the sum over all items of the (absolute) difference in positions for each item. (We also add a normalization factor so that the distance varies ranges from 0 to 1 but omit that trivia here.)

So the distance between A, B, C and A, B, C is 0; the (unnormalized) distance between A, B, C and C, B, A is 4; and the (unnormalized) distance between A, B, C and B, A, C is 2.
:::

## Kendall's tau

Another common distance metric between rankings is [Kendall's tau](https://en.wikipedia.org/wiki/Kendall_tau_distance). It's defined as:

::: .skippable
$$d_tau(u, v) = \sum_{\{i,j\} \in P} \bar{K}_{i,j}(u, v)$$

where:

- $u$ and $v$ are again rankings, 
- $i$ and $j$ are items in the set of unordered pairs $P$ of distinct elements in $u$ and $v$
- $\bar{K}_{i,j}(u, v) = 0$ if $i$ and $j$ are in the same order (concordant) in $u$ and $v$ and $\bar{K}_{i,j}(u, v) = 0$ otherwise (discordant)
:::

In other words, the Kendall tau distance looks at all possible pairs across items in the rankings and counts up the ones where the two rankings disagree on the ordering of these items. (There's also a normalization factor that we've again omitted so that the distance ranges from 0 to 1.)

## Angular distance

One drawback of the above metrics is that they throw away information in going from the table with cost-effectiveness estimates to a simple ranking. What would be ideal is to keep that information and find some other distance metric that still emphasizes the relationship between the various numbers rather than their precise values.

Angular distance is a metric which satisfies these criteria. We can regard the table of charities and cost-effectiveness values as an 8-dimensional vector. When our output produces another vector of cost-effectiveness estimates (one for each charity), we can compare this to our reference vector by finding [the angle between the two]{.noted}[^angle].

<!--more-->

# Results

## Uncertainties

To recap, what we're about to see next is the result of running our model many times with different sampled input values. In each run, we compute the cost-effectiveness estimates for each charity and compare those estimates to the reference ranking (GiveWell's best estimate) using each of the tau, footrule and angular distance metrics.

<figure class="natural-fig">
![Probability distributions of value per dollar for each of GiveWell's top charity and probability distributions for the distance between model results and the reference results](/images/givewell-analysis/uncertainties-small-multiples-distances.png)
<figcaption>Probability distributions of value per dollar for each of GiveWell's top charity and probability distributions for the distance between model results and the reference results</figcaption>
</figure>

We see that our input uncertainty does matter even for these highest level results---there are some input values which cause the ordering of best charities to change. If the gaps between the cost-effectiveness estimates had been very large or our input uncertainty had been very small, we would have expected essentially all of the probability mass to be concentrated at 0 because no change in inputs would have been enough to meaningfully change the relative value of the charities.

## Visual sensitivity analysis

We can now repeat our visual sensitivity analysis but using our [angular distance metric from the reference as our outcome of interest]{.noted}[^angular] instead of individual cost-effectiveness estimates. What these plots show is how sensitive the relative value of the different charities (the angle between the results and the reference vector) is to each of the input parameters used in any of the cost-effectiveness models (so, yes, there are a lot of parameters/plots).

<figure class="natural-fig">
![Scatter plots showing sensitivity of the overall cost-effective analysis to each input parameters](/images/givewell-analysis/regressions-max-overall%20ranking-angle.png)
<figcaption>Scatter plots showing sensitivity of the overall relative cost-effective analysis to each input parameter</figcaption>
</figure>

These results might be a bit surprising at first. Why are there so many charity-specific factors up top? Shouldn't input parameters which affect *all* models have the biggest influence on the overall result? Also, why do so few of the factors that showed up as most influential in [the charity-specific sensitivity analyses from last time](/posts/sensitivity-analysis-of-givewell-cea/) make it to the top?

However, after reflecting for a bit, this makes sense. Because we're interested in the *relative* performance of the charities, any factor which affects them all equally is of little importance here. Instead, we want factors that have a strong influence on only a few charities. When we go back to the earlier charity-by-charity sensitivity analysis, we see that many of the input parameters we identified as most influential where shared across charities (especially across the deworming charities). Non-shared factors that made it to the top of the charity-by-charity lists---like the relative risk of all-cause mortality for young children in <abbr title="Vitamin A supplementation">VAS</abbr> programs---show up somewhat high here too. 

But it's hard to eyeball the sensitivity when there are so many factors and most are of small effect. So let's move on to the delta analysis.

## Delta moment-independent sensitivity analysis

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the overall relative cost-effect analysis](/images/givewell-analysis/sensitivity-max-delta.png)
<figcaption>Delta sensitivities for each input parameter in the overall relative cost-effect analysis</figcaption>
</figure>

We see that (given our input uncertainty), the five parameters with the highest delta sensitivities are as described in the table below:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                               | Type of uncertainty | Meaning/importance                                                                                      |
| :---                                                                | :--                 | :-----                                                                                                  |
| cost per capita per annum (DTW and SCI)                             | Operational         | Affects cost of results                                                                                 |
| worm intensity adjustment (DTW and SCI)                             | Empirical/causal    | Affects size of problem and effect of deworming in areas with lower worm intensity than initial studies |
| relative risk of all-cause mortality for young children in programs | Causal              | How much do <abbr title="Vitamin A supplementation">VAS</abbr> programs affect mortality                |

It may be slightly surprising that cost per capita per annum and worm intensity adjustment are so much higher for <abbr title="Deworm the World">DTW</abbr> and <abbr title="Schistosomiasis Control Initiative">SCI</abbr> than these same factors for Sightsavers and the END Fund. This reflects the fact that <abbr title="Deworm the World">DTW</abbr> and <abbr title="Schistosomiasis Control Initiative">SCI</abbr> are assessed by GiveWell to be substantially more cost effective than Sightsavers and the END Fund.

# Conclusion

We started with a couple of problems with our previous analysis: we were taking cost-effectiveness estimates literally and looking at them independently instead of as a cohesive analysis. We addressed these problems by redoing our analysis while looking at distance metrics from the current best cost-effectiveness estimates as the outcomes of interest. We found that our input uncertainty is consequential even when looking only at the relative value of the charities. We also found that input parameters which our important but unique to a particular charity are most likely to affect the final output substantially.

Finally, we have the same caveat as list time: these results still reflect my fairly arbitrary (but scrupulously neutral) decision to pretend that we equally uncertain about each input parameter. To remedy this flaw, head over to the [Jupyter notebook](TODO) and tweak the input distributions.

# Appendix

We can also look at the sensitivities based on the Sobol method again.

The variable order in each plot is from the input parameter with the highest $\delta_i$ sensitivity to the input parameter with the lowest $delti_i$ sensitivity. That makes it straightforward to compare the ordering of sensitivities according to the delta moment-independent method and according to the Sobol method. We see that there is broad---but not perfect---agreement between the different methods.

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the overall relative cost-effect analysis](/images/givewell-analysis/sensitivity-max-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the overall relative cost-effect analysis</figcaption>
</figure>

[^ranking-probs]: If we just look at the probability for each possible ranking independently, we'll be overwhelmed by the number of permutations and it will be hard to find any useful structure in our results.
[^angle]: The angle between the vectors is a better metric here than the distance between the vectors' endpoints because we're interested in the relative values of the charities and how those change. If our results show that each charity is twice as effective as in the reference vector, our metric should return a distance of 0 because nothing has changed in the relative value of each charity.
[^angular]: I chose to use just one metric here because there are already enough plots and I chose angular distance instead of the others because the other two metrics produce integer outputs prior to normalization (which is inconvenient).
