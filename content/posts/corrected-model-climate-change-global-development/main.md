---
title: A corrected model suggests climate change interventions may be within a factor of two of direct cash transfers
date: 2019-11-25
tags: effective altruism, global warming
---

In [an earlier EA forum post](https://forum.effectivealtruism.org/posts/GEM7iJnLeMkTMRAaf/updated-global-development-interventions-are-generally-more), data from a new paper on [country-level social cost of carbon](https://www.nature.com/articles/s41558-018-0282-y) is used to estimate the comparative cost-effectiveness of climate change interventions and global development interventions (cost-effectiveness of the latter as determined by [GiveWell](https://www.givewell.org/)'s models). A central component of the earlier post (henceforth [<abbr title="Global development vs climate change">GDvCC</abbr>]{.noted}[^gdvcc]) is supposed to be [income]{.noted}[^income-consumption]-weighting---$10 dollars of lost income means a great deal more in terms of utility for someone that makes $200 per year than for someone that makes $20,000 per year. More granular, country-level data on the social cost of carbon gets us [closer to accounting for this distributional consideration]{.noted}[^full-account] and allows us to express the social cost of carbon in a way that's directly comparable with the outputs of GiveWell's models. [GDvCC's model](https://docs.google.com/spreadsheets/d/144pDOA26jscRAaARcLs9KVMUAjNl5vx_nJ-z7tahu0A/) finds that, in its "realistic" scenario, climate change interventions are 3% as cost-effective as GiveDirectly and 0.4% as cost-effective as GiveWell's median top charity. However, I think there's an error in the way the country-level social cost of carbon (CSCC) is interpreted in GDvCC which leads to incorrect income-weighting. Correcting for this error suggests that climate change interventions (again, under "realistic" assumptions) are 57% as cost-effective as GiveDirectly.

[^gdvcc]: I'm referring to it as GDvCC to keep the focus on the ideas rather than the people involved. Thank you to the original author for the work he did---most of which I am reusing---and for discussion in the comments on the original post.
[^income-consumption]: Apologies if I get a bit sloppy with "income" vs "consumption". The thing we care about directly as far as welfare is concerned is consumption, but often income data is all that's available. I'll also use "income" to match the language in a source under discussion. The two are generally closely correlated over the long-run but may diverge due to things like subsidies (e.g. [SNAP](https://en.wikipedia.org/wiki/Supplemental_Nutrition_Assistance_Program) in the U.S. counts for consumption but not income.). Also, I'll pretend GDP per capita is the same as income which is not strictly correct.
[^full-account]: A full account of this consideration would entail having household- or individual-level consumption data and doing the weighting on that basis. The discrepancy between country-level and household-level weighting increases as within-country consumption inequality increases.

<!--more-->

# Interpreting country-level social cost of carbon: the discrepancy

Interpreting CSCC data seems to be difficult (for example, there was also an earlier version of GDvCC which interpreted it in a third way). Given that, I'm going to explain my interpretation and how it differs from that in GDvCC in a variety of ways---once/if one of the explanations works for you, feel free to skip the rest. After this section, we'll talk about why I prefer my interpretation.

## Concrete example

In one of GDvCC's author's [comments](https://forum.effectivealtruism.org/posts/GEM7iJnLeMkTMRAaf/updated-global-development-interventions-are-generally-more#4BRx3vABoWjhr3kHa), he explains that, "There you can for instance see that India's SCC is $85.4 . [...] This figure is then income adjusted so that it is comparable with present day Americans[.]". I don't see any reason to believe that that adjustment-to-America happens in the CSCC data. On my interpretation, an $85 SCC in India means that if we could rearrange the social costs of some single tonne of carbon so that they fell entirely on an Indian of average income---roughly $2000---the welfare-equivalent income is $1915. In other words, the victim of our strange scenario would be roughly indifferent between an income of $1915 without the social costs of that tonne of carbon and $2000 with the social costs of that tonne of carbon. In the GDvCC interpretation, the welfare-equivalent income is roughly $1997.5. This results from believing that the original $85/tonne figure is quoted in dollars pegged to American average income and then dividing by 34 to account for the lower income in India and the corresponding fact that a dollar "means more" there ($34 = \frac{62641}{2016}^{1.5}$ which follows the calculations in GDvCC's model and sets $\eta$---a parameter expressing how the value of a marginal dollar changes with income---to 1.5).

## Data-based model

Because the GDvCC post operates on the belief that all the country-level social costs of carbon have already been "income adjusted so that [they are] comparable with present day Americans", it simply applies a uniform poverty multiplier (1260) to translate these American-adjusted dollars to GiveDirectly-recipient dollars. On my interpretation, the country-level social costs of carbon have not already been adjusted to some common numeraire so each CSCC must be adjusted using a separate poverty multiplier which accounts for that country's average income. Using [supplementary data from *Country-level social cost of carbon*](https://static-content.springer.com/esm/art%3A10.1038%2Fs41558-018-0282-y/MediaObjects/41558_2018_282_MOESM2_ESM.csv) and [GDP per capita data from the World Bank](https://data.worldbank.org/indicator/NY.GDP.PCAP.CD), I've produced a quick [spreadsheet model](https://docs.google.com/spreadsheets/d/1Xb7K-8uW3cTGamUoErgkxlhqD1LaGJjRJtbWXmOz-1Y/) of these interpretations. 

The "GDvCC analogue" tab replicates the calculations performed in the original post using this more granular data---it applies the uniform "realistic" poverty multiplier to each country and then sums rather than applying the poverty multiplier after summing. Reassuringly, the estimated global cost of carbon matches that found in the "realistic" scenario in GDvCC's model---$0.32. The key part to notice is that the poverty multiplier is the same for each country.

The "CSCC in American dollars" tab gives each country a separate poverty multiplier using American GDP per capita as the reference income. The multiplier is 1 for America, less than 1 for countries richer than America and much greater than 1 for countries much poorer than America. Applying each country's poverty multiplier to its country-level social cost of carbon and summing gives us a global social cost of carbon $36,834. If 100% of the social cost of a tonne of CO2 fell on an American making $62,641 (US GDP per capita), this would be the welfare-equivalent of a $36,834 reduction in income.

The "CSSC in GiveDirectly dollars" tab follows the same procedure but uses GiveDirectly annual consumption ($180) as the reference income. Thus the poverty multiplier is near 0 for most developed countries and is only over 0.5 for extremely poor countries like Burundi. Applying each country's poverty multiplier to its country-level social cost of carbon and summing gives us a global social cost of carbon of $5.67. If 100% of the cost of a tonne of CO2 fell on a typical GiveDirectly recipient, this would be the welfare-equivalent of a $5.67 reduction in income.

(Both the "CSCC in American dollars" tabs and "CSCC in GiveDirectly dollars" models express valid calculations. They just produce final figures expressed in terms of different numeraires. We can in fact convert between the two using a poverty multiplier of 6,492 (conceptually the same as the 1260 appearing in GDvCC but $\frac{\$62,641}{$180}^{1.5}$ instead of $\frac{\$21,000}{$180}^{1.5}$ because of our World Bank GDP per capita data for US income instead of median).)

## Unit analysis

We can also think of this a bit more abstractly in terms of unit analysis. This is a generally useful way to check models by making sure that all the units of measure line up; we don't want to add 20 seconds to 4 dollars because the resulting quantity doesn't have any sensible physical interpretation. Once we realize that not all dollars are equal and we should treat dollars of consumption for the average Indian differently than dollars of consumption for the average American, we can get a lot of mileage out of this approach.

The global social cost of carbon when constructed from the country-level estimates is effectively $CC_G = CC_a + CC_b + CC_c + \ldots$ where $CC_G$ is the global social cost of carbon and $CC_a$, $CC_b$, $CC_c$, etc. are the country level social costs of carbon for countries 'a', 'b', 'c', etc.

The GDvCC model then uses a poverty multiplier of 1,260 expressing that a dollar means more to a poor person than a rich person. We can also think of this as a unit conversion factor: 1,260 American median income dollars = 1 GiveDirectly recipient dollar. We write this conversion factor as $\frac{1260 \$_{A} }{1 \$_{GD} } = 1$ where $\$_{A}$ is a dollar in America and $\$_{GD}$ is a dollar for a GiveDirectly recipient.

The next step in the GDvCC model divides the global social cost of carbon by the uniform poverty multiplier. If we expand global social cost of carbon, the calculation looks like: $\frac{CC_G}{1260} = \frac{CC_a}{1260} + \frac{CC_b}{1260} + \frac{CC_c}{1260} + \ldots$.

If we just look at the units, this is $\$_G \frac{\$_{GD}}{\$_{A}} = \$_a \frac{\$_{GD}}{\$_A} + \$_b \frac{\$_{GD}}{\$_A} + \$_c \frac{\$_{GD}}{\$_A} + \ldots$

where $\$_G$ is a country-level-cost-of-carbon-weighted dollar, $\$_a$ is a dollar in country 'a', etc. Converting country 'a' dollars to GiveDirectly recipient dollars via $\frac{1\$_{GD}}{1260 \$_A}$ is only appropriate if country 'a' is in fact America. Otherwise, the units don't line up.

The "CSCC in GiveDirectly dollars" model looks like $CC_{G-GD} = \$_a \frac{\$_{GD}}{\$_a} + \$_b \frac{\$_{GD}}{\$_b} + \$_c \frac{\$_{GD}}{\$_c} + \ldots$ where $CC_{G-GD}$ is the global social cost of carbon expressed in GiveDirectly recipient dollars. In this case numerator and denominator of each term on the right-hand side cancel leaving us with a global social cost of carbon expressed in terms of GiveDirectly dollars.

**To summarize**, unit analysis suggests that simply adding the country-level social costs of carbon and then applying a single poverty multiplier (as the GDvCC model does) is inappropriate because the concept of a dollar of consumption actually disguises substantial heterogeneity across countries. We need to homogenize the units with tailored poverty multipliers before summing the country-level social costs of carbon is a sensible operation.

# Which interpretation to prefer?

## Examination of study methodology

My basic argument against the GDvCC interpretation is that the adjustment-to-America it supposes is an additional step that is not described anywhere in the paper and doesn't seem to naturally fit in any component of the model as described. Furthermore, I don't think this adjustment is so common and expected as to not merit mention. Thus, the absence of evidence is evidence of absence.

[*Country-level social cost of carbon*](https://www.nature.com/articles/s41558-018-0282-y) and social cost of carbon models generally, as far as I understand, consist of four modules:

> - a socio-economic module wherein the future evolution of the economy, which includes the projected emissions of CO2, is characterized without the impact of climate change;
> - a climate module wherein the earth system responds to emissions of CO2 and other anthropogenic forcings;
> - a damages module, wherein the economy’s response to changes in the Earth system are quantified; and 
> - a discounting module, wherein a time series of future damages is compressed into a single present value.

Of these four modules, damages and discounting seem like the only places where the required adjustment could happen.

I don't think that "income [is] adjusted so that it is comparable with present day Americans" in the damages module. It doesn't seem to fit the description of the damages module which talks about "the economy's response to changes in the Earth system". Indeed, a quick skim of the damage function paper [referenced](http://sci-hub.tw/https://www.nature.com/articles/nature15725) in [*Country-level social costs of carbon*](https://www.nature.com/articles/s41558-018-0282-y) shows it to be talking purely about macroeconomic indicators. The referenced paper says "The impact of warming on global economic production is a population-weighted average of country-level impacts in Fig. 4a." where Fig 4a is about "Change in GDP per capita (RCP8.5, SSP5) relative to projection". So the output of the damages module in that paper is a population-weighted function of local, purely economic impacts (e.g. a 1% decline in India's GDP).

The discounting module seems like the most likely candidate location for this sort of adjustment to happen. The discounting module already handles temporal discounting and we can think of the required adjustments as geographic or income discounting. But, in this case, I don't think the discounting module actually includes these adjustments. The high-level description of the module---"wherein a time series of future damages is compressed into a single present value"---describes only temporal discounting. Looking at the methodology in more detail, the only mentions of income-related adjustments are: (1) an optional rich-poor damage specification which affects how damages grow over time for countries in each bin; (2) an optional elasticity of marginal utility adjustment to account for how the effective social cost diminishes [as economies grow]{.noted}[^across-time]. So, as expected, all the particulars of the methodology are about how income effects change over time. None of the mentioned adjustments take account of differing incomes and consumption at the beginning of the model i.e. the present day.

[^across-time]: I think it's pretty clear from context that this income adjustment is only applied within countries over time rather than across countries at time T=0: "We thus used growth-adjusted discounting determined by the Ramsey endogenous rule, with a range of values for the elasticity of marginal utility (μ) and the pure rate of time preference (ρ), but we also report fixed discounting results to demonstrate the sensitivity of SCC calculations to discounting methods."

## Sanity check on data

A quick inspection of the country-level data also provides independent reason to doubt that the country-level social costs have already been income-adjusted. For example, it strikes me as fairly implausible that India's income-adjusted CSCC is less than twice the United States' ($85 vs $47) given that India has 4 times the population, a climate much more susceptible to the negative impacts of global warming, and a much poorer population.

# Conclusion

If we accept the above arguments the country-level social costs of carbon are each expressed in local terms and the rough, corrected model which accounts for this suggests that the social cost of carbon expressed in terms of income for a typical GiveDirectly recipient is $5.67. In other words, a typical GiveDirectly recipient would be indifferent between bearing the full social costs of a tonne of carbon with an annual consumption of $180 and bearing none of the costs with a consumption of $174.33. If we use GDvCC's "realistic" estimate of $10/tonne CO2 averted, this means that such climate change interventions produce 57% as much benefit per dollar donated as a donation to GiveDirectly.
