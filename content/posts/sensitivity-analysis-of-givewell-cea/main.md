---
title: Sensitivity analysis of GiveWell's cost-effectiveness analysis
date: 2019-09-28
tags: statistics, development
series: GiveWell cost-effectiveness analysis analysis
---

[Last time](/posts/uncertainty-analysis-of-givewell-cea/) we introduced GiveWell's cost effectiveness analysis which uses a spreadsheet model to take point estimates of uncertain input parameters to point estimates of uncertain results. We adjusted this approach to take probability distributions on the input parameters and in exchange got probability distributions on the resulting cost-effectiveness estimates. But this machinery lets us do more. Now that we've completed an uncertainty analysis, we can move on to sensitivity analysis.

# Sensitivity analysis

The basic idea of [sensitivity analysis](https://en.wikipedia.org/wiki/Sensitivity_analysis) is, when working with uncertain values, to see which input values most affect the output when they vary. For example, if you have the equation $f(a, b) = 2^a + b$ and each of $a$ and $b$ varies uniformly over the range from 5 to 10, $f(a, b)$ is much more sensitive to $a$ then $b$. A sensitivity analysis is practically useful in that it can offer you guidance as to which parameters in your model it would be most useful to investigate further (i.e. to narrow their uncertainty).

# Visual sensitivity analysis

The first kind of sensitivity analysis we'll run is just to look at scatter plots comparing each input parameter to the final cost-effectiveness estimates. We can imagine these scatter plots as the result of running [the following procedure many times]{.noted}[^monte-carlo]: sample a single value from the probability distribution for each input parameter and run the calculation on these values to determine a result value. If we repeat this procedure enough times, it starts to approximate the true values of the probability distributions. 

One nice feature of this sort of analysis is that we see how the output depends on a particular input even in the face of variations in all the other inputs---we don't hold everything else constant. In other words, this is a [global](https://en.wikipedia.org/wiki/Sensitivity_analysis#Local_methods) sensitivity analysis.

(Caveat: We are again pretending that we are equally uncertain about each input parameter and the results reflect this limitation. To see the analysis result for different input uncertainties, edit and run [the Jupyter notebook](TODO).)

## Direct cash transfers

### GiveDirectly

<figure class="natural-fig">
![Scatter plots showing sensitivity of GiveDirectly's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-GiveDirectly-value_per_dollar.png)
<figcaption>Scatter plots showing sensitivity of GiveDirectly's cost-effectiveness to each input parameter</figcaption>
</figure>

The scatter plots show that, given our choice of input uncertainty, the output is most sensitive (i.e. the scatter plot for these parameters shows the greatest directionality) to the input parameters:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                   | Type of uncertainty                   | Meaning/importance                                                                 |
| :---                                                    | :--                                   | :----                                                                              |
| value of increasing ln consumption per capita per annum | Moral                                 | Determines final conversion between outcomes and value                             |
| transfer as percent of total cost                       | Operational                           | Determines cost of results                                                         |
| return on investment                                    | Opportunities available to recipients | Determines stream of consumption over time                                         |
| baseline consumption per capita                         | Empirical                             | Diminishing marginal returns to consumption mean that baseline consumption matters |
</figure>

<!--more-->

## Deworming

Some useful and non-obvious context for the following is that the primary claimed benefit of deworming is increased income later in life.

### The END Fund

<figure class="natural-fig">
![Scatter plots showing sensitivity of the END Fund's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-END-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of the END Fund's cost-effectiveness to each input parameter</figcaption>
</figure>

Here, it's a little harder to identify certain factors as more important. It seems that the final estimate is (given our input uncertainty) the result of many factors of medium effect. It does seem plausible that the output is somewhat less sensitive to these factors:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to the END Fund shift around other money |
</figure>

### Deworm the World

<figure class="natural-fig">
![Scatter plots showing sensitivity of Deworm the World's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-DTW-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of the Deworm the World's cost-effectiveness to each input parameter</figcaption>
</figure>

Again, it's a little harder to identify certain factors as more important. It seems that the final estimate is (given our input uncertainty) the result of many factors of medium effect. It does seem plausible that the output is somewhat less sensitive to these factors:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to Deworm the World shift around other money |
</figure>

### Schistosomiasis Control Initiative

<figure class="natural-fig">
![Scatter plots showing sensitivity of Schistosomiasis Control Initiative's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-SCI-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of the Schistosomiasis Control Initiative's cost-effectiveness to each input parameter</figcaption>
</figure>

Again, it's a little harder to identify certain factors as more important. It seems that the final estimate is (given our input uncertainty) the result of many factors of medium effect. It does seem plausible that the output is somewhat less sensitive to these factors:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to Schistosomiasis Control Initiative shift around other money |
</figure>

### Sightsavers

<figure class="natural-fig">
![Scatter plots showing sensitivity of Sightsavers' cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-Sightsavers-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of the Sightsavers' cost-effectiveness to each input parameter</figcaption>
</figure>

Again, it's a little harder to identify certain factors as more important. It seems that the final estimate is (given our input uncertainty) the result of many factors of medium effect. It does seem plausible that the output is somewhat less sensitive to these factors:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to Sightsavers shift around other money |
</figure>

## Seasonal malaria chemoprevention

### Malaria Consortium

<figure class="natural-fig">
![Scatter plots showing sensitivity of Malaria Consortium's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-Malaria%20Consortium-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of Malaria Consortium's cost-effectiveness to each input parameter</figcaption>
</figure>

The scatter plots show that, given our choice of input uncertainty, the output is most sensitive (i.e. the scatter plot for these parameters shows the greatest directionality) to the input parameters:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                        | Type of uncertainty       | Meaning/importance                                                                                                                   |
| :----                                        | :--                       | :--------                                                                                                                            |
| direct mortality in high transmission season | Empirical                 | Fraction of overall malaria mortality  during the peak transmission season and amenable to SMC                                       |
| internal validity adjustment                 | Methodological            | How much do we trust the results of the underlying <abbr title="Seasonal malaria chemoprevention">SMC</abbr> studies)                |
| external validity adjustment                 | Methodological            | How much do the results of the underlying <abbr title="Seasonal malaria chemoprevention">SMC</abbr> studies transfer to new settings |
| coverage in trials in meta-analysis          | Historical/methodological | Determines how much coverage an SMC program needs to achieve to match studies                                                        |
| value of averting death of a young child     | Moral                     | Determines final conversion between outcomes and value                                                                               |
| cost per child targeted | Operational | Affects cost of results |
</figure>

## Vitamin A supplementation

### Helen Keller International

<figure class="natural-fig">
![Scatter plots showing sensitivity of Helen Keller International's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-HKI-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of the Helen Keller International's cost-effectiveness to each input parameter</figcaption>
</figure>

The scatter plots show that, given our choice of input uncertainty, the output is most sensitive (i.e. the scatter plot for these parameters shows the greatest directionality) to the input parameters:


<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                               | Type of uncertainty | Meaning/importance                                                                       |
| :-----                                                              | :--                 | :---                                                                                     |
| relative risk of all-cause mortality for young children in programs | Causal              | How much do <abbr title="Vitamin A supplementation">VAS</abbr> programs affect mortality |
| cost per child per round                                            | Operational         | Affects the total cost required to achieve effect                                        |
| rounds per year                                                     | Operational         | Affects the total cost required to achieve effect                                        |
</figure>

## Bednets

### Against Malaria Foundation

<figure class="natural-fig">
![Scatter plots showing sensitivity of Against Malaria Foundation's cost-effectiveness to each input parameter](/images/givewell-analysis/regressions-big-AMF-value_per_dollar_w_levfun.png)
<figcaption>Scatter plots showing sensitivity of Against Malaria Foundation's cost-effectiveness to each input parameter</figcaption>
</figure>

The scatter plots show that, given our choice of input uncertainty, the output is most sensitive (i.e. the scatter plot for these parameters shows the greatest directionality) to the input parameters:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                                               | Type of uncertainty  | Meaning/importance                                                                                                      |
| :---                                                                                | :--                  | :-----                                                                                                                  |
| num <abbr title="Long-lasting insecticidal net">LLIN</abbr>s distributed per person | Operational          | Affects the total cost required to achieve effect                                                                       |
| cost per <abbr title="Long-lasting insecticidal net">LLIN</abbr>                    | Operational          | Affects the total cost required to achieve effect                                                                       |
| deaths averted per protected child under 5                                          | Causal               | How effective is the core activity                                                                                      |
| lifespan of an <abbr title="Long-lasting insecticidal net">LLIN</abbr>              | Empirical            | Determines how many years of benefit accrue to each distribution                                                        |
| net use adjustment                                                                  | Empirical            | Determines benefits from <abbr title="Long-lasting insecticidal net">LLIN</abbr> as mediated by proper and improper use |
| internal validity adjustment                                                        | Methodological       | How much do we trust the results of the underlying studies                                                              |
| percent of mortality due to malaria in AMF areas vs trials                          | Empirical/historical | Affects size of the problem                                                                                   |
| percent of pop. under 5                                                             | Empirical            | Affects size of the problem                                                                                   |
</figure>

# Delta moment-independent sensitivity analysis

If eyeballing plots seems a bit unsatisfying to you as a method for judging sensitivity, not to worry. We also have the results of a more formal sensitivity analysis. This method is called [delta moment-independent sensitivity analysis](http://www.relialab.org/Upload/files/A%20new%20uncertainty%20importance%20measure.pdf).

$\delta_i$ (the delta moment-independent sensitivity indicator of parameter $i$) "represents the normalized expected shift in the distribution of [the output] provoked by [that input]" [@borgonovo2007new]. To make this meaning more explicit, we'll start with some notation/definitions. Let:

::: {.skippable}
1. $X = (X_1, X_2, \ldots, X_n) \in \mathbb{R}^n$ be the random variables used as input parameters
2. $Y = f(X)$ so that $f(X)$ is a function from $\mathbb{R}^n$ to $\mathbb{R}$ describing the relationship between inputs and outputs---i.e. GiveWell's cost-effectiveness model
<!-- 3. $x = (x_1, x_2, \ldots, x_n)$ be a realization of X---i.e. some particular value for each of the input random variables -->
<!-- 4. $f_X(x)$ be the joint density of $X$---i.e. the multi-dimensional probability distribution over all input parameters simultaneously -->
<!-- 5. $f_{X_i}(x_i)$ be the marginal density of $x_i$---i.e. the individual probability distributions for each input parameter that we've already talked about -->
3. $f_Y(y)$ be the density function of the result $Y$---i.e. the probability distributions we've already seen showing the cost-effectiveness for each charity
4. $f_{Y|X_i}(y)$ be the conditional density of Y with one of the parameters $X_i$ fixed---i.e. a probability distribution for the cost-effectiveness of a charity while pretending that we know one of the input values precisely

With these in place, we can define $\delta_i$. It is:

$$\delta_i = \frac{1}{2} E_{X_i}[\int |f_Y(y) - f_{Y|X_i}(y)| \mathrm{d}y]$$.

The inner $\int |f_Y(y) - f_{Y|X_i}(y)| \mathrm{d}y$ can be interpreted as the total area between probability density function $f_Y$ and probability density function $f_{Y|X_i}$. This is the "shift in the distribution of $Y$ provoked by $X_i$" we mentioned earlier. Overall, $\delta_i$ then says:

- pick one value for $X_i$ and measure the shift in the output distribution from the "default" output distribution
- do that for each possible $X_i$ and take the expectation
:::

Some useful properties to point out:

- $\delta_i$ ranges from 0 to 1
- If the output is independent of the input, $delta_i$ for that input is 0
- The sum of $\delta_i$ for each input considered separately isn't necessarily 1 because there can be interaction effects

## Direct cash transfers

### GiveDirectly

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the GiveDirectly cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-GiveDirectly-delta.png)
<figcaption>Delta sensitivities for each input parameter in the GiveDirectly cost-effectiveness calculation</figcaption>
</figure>

Comfortingly, this agrees with the results of our scatter plot sensitivity analysis. For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                   | Type of uncertainty                   | Meaning/importance                                                                 |
| :---                                                    | :--                                   | :----                                                                              |
| value of increasing ln consumption per capita per annum | Moral                                 | Determines final conversion between outcomes and value                             |
| transfer as percent of total cost                       | Operational                           | Affects cost of results                                                  |
| return on investment                                    | Opportunities available to recipients | Determines stream of consumption over time                                         |
| baseline consumption per capita                         | Empirical                             | Diminishing marginal returns to consumption mean that baseline consumption matters |
</figure>

## Deworming

### The END Fund

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the END Fund cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-END-delta.png)
<figcaption>Delta sensitivities for each input parameter in the END Fund cost-effectiveness calculation</figcaption>
</figure>

[Comfortingly, this again agrees with the results of our scatter plot sensitivity analysis]{.noted}[^cheat]. For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to the END Fund shift around other money |
</figure>

### Deworm the World

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the Deworm the World cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-DTW-delta.png)
<figcaption>Delta sensitivities for each input parameter in the Deworm the World cost-effectiveness calculation</figcaption>
</figure>

For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to Deworm the World shift around other money |
</figure>

### Schistosomiasis Control Initiative

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the Schistosomiasis Control Initiative cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-SCI-delta.png)
<figcaption>Delta sensitivities for each input parameter in the Schistosomiasis Control Initiative cost-effectiveness calculation</figcaption>
</figure>

For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to Schistosomiasis Control Initiative shift around other money |
</figure>

### Sightsavers

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the Sightsavers cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-Sightsavers-delta.png)
<figcaption>Delta sensitivities for each input parameter in the Sightsavers cost-effectiveness calculation</figcaption>
</figure>

For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is minimally sensitive</figcaption>
| Input                                    | Type of uncertainty | Meaning/(un)importance                                               |
| :---                                     | :--                 | :-----                                                               |
| num yrs between deworming and benefits   | Forecast            | Affects how much discounting of future income streams must be done   |
| duration of long-term benefits           | Forecast            | The length of time for a which a person works and earns income       |
| expected value from leverage and funging | Game theoretic      | How much does money donated to Sightsavers shift around other money |
</figure>

### Deworming comment

That we get substantially identical results in terms of delta sensitivities for each deworming charity is not surprising: The structure of each calculation is the same and (for the sake of tainting the analysis with my idiosyncratic perspective) the uncertainty on each input parameter is the same.

## Seasonal malaria chemoprevention

### Malaria Consortium

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the Malaria Consortium cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-Malaria%20Consortium-delta.png)
<figcaption>Delta sensitivities for each input parameter in the Malaria Consortium cost-effectiveness calculation</figcaption>
</figure>

Again, there seems to be good agreement between the delta sensitivity analysis and the scatter plot sensitivity analysis though there is perhaps a bit of reordering in the top factor. For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                        | Type of uncertainty       | Meaning/importance                                                                                                                   |
| :----                                        | :--                       | :--------                                                                                                                            |
| internal validity adjustment                 | Methodological            | How much do we trust the results of the underlying <abbr title="Seasonal malaria chemoprevention">SMC</abbr> studies)                |
| direct mortality in high transmission season | Empirical                 | Fraction of overall malaria mortality  during the peak transmission season and amenable to SMC                                       |
| cost per child targeted                      | Operational               | Afffects cost of results                                                                                                   |
| external validity adjustment                 | Methodological            | How much do the results of the underlying <abbr title="Seasonal malaria chemoprevention">SMC</abbr> studies transfer to new settings |
| coverage in trials in meta-analysis          | Historical/methodological | Determines how much coverage an SMC program needs to achieve to match studies                                                        |
| value of averting death of a young child     | Moral                     | Determines final conversion between outcomes and value                                                                               |
</figure>

## Vitamin A supplementation

### Hellen Keller International

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the Helen Keller International cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-HKI-delta.png)
<figcaption>Delta sensitivities for each input parameter in the Helen Keller International cost-effectiveness calculation</figcaption>
</figure>

Again, there's broad agreement between the scatter plot analysis and this one. This analysis perhaps makes the crucial importance of the relative risk of all-cause mortality for young children in VAS programs even more obvious. For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                               | Type of uncertainty | Meaning/importance                                                                       |
| :-----                                                              | :--                 | :---                                                                                     |
| relative risk of all-cause mortality for young children in programs | Causal              | How much do <abbr title="Vitamin A supplementation">VAS</abbr> programs affect mortality |
| cost per child per round                                            | Operational         | Affects the total cost required to achieve effect                                        |
| rounds per year                                                     | Operational         | Affects the total cost required to achieve effect                                        |
</figure>

## Bednets

### Against Malaria Foundation

<figure class="natural-fig">
![Delta sensitivities for each input parameter in the Against Malaria Foundation cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-AMF-delta.png)
<figcaption>Delta sensitivities for each input parameter in the Against Malaria Foundation cost-effectiveness calculation</figcaption>
</figure>

Again, there's broad agreement between the scatter plot analysis and this one. For convenience, I have copied the table from the scatter plot analysis describing the most influential inputs:

<figure class="big-fig">
<figcaption>Highlighted input factors to which result is highly sensitive</figcaption>
| Input                                                                               | Type of uncertainty  | Meaning/importance                                                                                                      |
| :---                                                                                | :--                  | :-----                                                                                                                  |
| num <abbr title="Long-lasting insecticidal net">LLIN</abbr>s distributed per person | Operational          | Affects the total cost required to achieve effect                                                                       |
| cost per <abbr title="Long-lasting insecticidal net">LLIN</abbr>                    | Operational          | Affects the total cost required to achieve effect                                                                       |
| deaths averted per protected child under 5                                          | Causal               | How effective is the core activity                                                                                      |
| lifespan of an <abbr title="Long-lasting insecticidal net">LLIN</abbr>              | Empirical            | Determines how many years of benefit accrue to each distribution                                                        |
| net use adjustment                                                                  | Empirical            | Affects benefits from <abbr title="Long-lasting insecticidal net">LLIN</abbr> as mediated by proper and improper use |
| internal validity adjustment                                                        | Methodological       | How much do we trust the results of the underlying studies                                                              |
| percent of mortality due to malaria in AMF areas vs trials                          | Empirical/historical | Affects size of the problem                                                                                   |
| percent of pop. under 5                                                             | Empirical            | Affects size of the problem                                                                                   |
</figure>

# Conclusion

We performed visual (scatter plot) sensitivity analyses and delta moment-independent sensitivity analyses on GiveWell's top charities. Conveniently, these two methods generally agreed as to which input factors had the biggest influence on the output. For each charity, we found that there were clear differences in the sensitivity indicators for different inputs. This suggests that certain inputs ought to be targeted for uncertainty reduction to improve GiveWell's cost-effectiveness analyses. For example, the overall estimate of the cost-effectiveness of Helen Keller International's vitamin A supplementation program depends much more on the relative risk of all-cause mortality for children in <abbr title="Vitamin A supplementation">VAS</abbr> programs than it does on the expected value from leverage and funging. If the cost of investigating each were the same, it would be better to spend time on the former.
Our sensitivity analysis has suggested which varies

An important caveat to remember is that these results still reflect my fairly arbitrary (but scrupulously neutral) decision to pretend that we equally uncertain about each input parameter. To remedy this flaw, head over to the [Jupyter notebook](TODO) and tweak the input distributions.

# Appendix

I also did a [variance-based sensitivity analysis](https://en.wikipedia.org/wiki/Variance-based_sensitivity_analysis) with Sobol indices. Those plots follow.

The variable order in each plot is from the input parameter with the highest $\delta_i$ sensitivity to the input parameter with the lowest $delti_i$ sensitivity. That makes it straightforward to compare the ordering of sensitivities according to the delta moment-independent method and according to the Sobol method. We see that there is broad---but not perfect---agreement between the different methods.

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the GiveDirectly cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-GiveDirectly-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the GiveDirectly cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the END Fund cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-END-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the END Fund cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the Deworm the World cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-DTW-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the Deworm the World cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the Schistosomiasis Control Initiative cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-SCI-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the Schistosomiasis Control Initiative cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the Sightsavers cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-Sightsavers-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the Sightsavers cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the Malaria Consortium cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-Malaria%20Consortium-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the Malaria Consortium cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the Helen Keller International cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-HKI-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the Helen Keller International cost-effectiveness calculation</figcaption>
</figure>

<figure class="natural-fig">
![Sobol sensitivities for each input parameter in the Against Malaria Foundation cost-effectiveness calculation](/images/givewell-analysis/sensitivity-big-AMF-s1.png)
<figcaption>Sobol sensitivities for each input parameter in the Against Malaria Foundation cost-effectiveness calculation</figcaption>
</figure>

<hr class="references">

[^monte-carlo]: This is, in fact, approximately what Monte Carlo methods do so this is a very convenient analysis to run.
[^cheat]: I swear I didn't cheat by just picking the results on the scatter plot that match the delta sensitivities!
