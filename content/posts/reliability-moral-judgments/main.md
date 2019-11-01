---
title: "The reliability of moral judgments: A survey and systematic(ish) review"
date: 2019-10-31
tags: ethics, statistics, review
include-toc: true
bibliography: ../../misc/biblio.bib
csl: ../../misc/biblio.csl
---

<!-- Otherwise the sidenotes overlap with tables and I can't be bothered to fix it right now. -->
<span class="disable-sidenotes" style="display: none;"></span>

(This post is painfully long. Coping advice: Each subsection within [Direct (empirical) evidence](#direct-empirical-evidence), within [Indirect evidence](#indirect-evidence), and within [Responses](#responses) is pretty independent---feel free to dip in and out as desired. I've also put a list-formatted summary at the end of each these sections boiling down each subsection to one or two sentences.)

# Intro

<blockquote>
Dan is a student council representative at his school. This semester he is in charge of scheduling discussions about academic issues. He often picks topics that appeal to both professors and students in order to stimulate discussion.
</blockquote>

Is Dan's behavior morally acceptable? On first glance, you'd be inclined to say yes. And even on the second and third glance, obviously, yes. Dan is a stand-up guy. But what if you'd been experimentally manipulated to feel disgust while reading the vignette? If we're to believe [@wheatley2005hypnotic], there's a one-third chance you'd judge Dan as morally suspect. 'One subject justified his condemnation of Dan by writing "it just seems like he’s up to something." Another wrote that Dan seemed like a "popularity seeking snob."'

The possibility that moral judgments track irrelevant factors like incidental disgust at the moment of evaluation is (to me, at least) alarming. But now that you've been baited, we can move on the boring, obligatory formalities.

<!--more-->

## What are moral judgments?

A moral judgment is a belief that some moral proposition is true or false. It is the output of a process of moral reasoning. When I assent to the claim "Murder is wrong.", I'm making a moral judgment.

::: {.skippable}
Quite a bit of work in this area talks about moral intuitions rather than moral judgments. Moral intuitions are more about the immediate sense of something than about the all-things-considered, reflective judgment. One model of the relationship between intuitions and judgments is that intuitions are the raw material which are refined into moral judgments by more sophisticated moral reasoning. We will talk predominately about moral judgments because: 

1. It's hard to get at intuitions in empirical studies. I don't have much faith in directions like "Give us your immediate reaction.".
2. Moral judgments are ultimately what we care about insofar as we call the things that motivate moral action moral judgments. 
3. It's not clear that moral intuitions and moral judgments are always distinct. There is reason to believe that, at least for some people some of the time, moral intuitions are not refined before becoming moral judgments. Instead, they are simply accepted at face value.

On the other hand, in this post, we are interested in judgmental unreliability driven by intuitional unreliability. We won't focus on additional noise that any subsequent moral reasoning may layer on top of unreliability in moral intuitions.
:::

## What would it mean for moral judgments to be unreliable?

The simplest case of unreliable judgments is when precisely the same moral proposition is evaluated differently at different times. If I tell you that "Murder is wrong in context A." today and "Murder is right in context A." tomorrow, my judgments are very unreliable indeed.

A more general sort of unreliability is when our moral judgments as actually manifested track factors that seem, upon reflection, [morally irrelevant]{.noted}[^time]. In other words, if two propositions are identical on all factors that we endorse as morally relevant, our moral judgments about these propositions should be identical. The fear is that, in practice, our moral judgments do not always adhere to this rule because we pay undue attention to other factors.

[^time]: The first, simplest sort of unreliability can be subsumed in this framework by considering the time of evaluation as a morally irrelevant factor.

These influential but morally irrelevant factors (attested to varying degrees in the literature as we'll see below) include things like: 

Order
:   The moral acceptability of a vignette depends on the order in which it's presented relative to other vignettes.
Disgust and cleanliness
:   The moral acceptability of a vignette depends how disgusted or clean the moralist (i.e. the person judging moral acceptability) feels at the time.

(The claim that certain factors are morally irrelevant is itself part of a moral theory. However, some factors seem to be morally irrelevant on a *very* wide range of moral theories.)

## Why do we care about the alleged unreliability of moral judgments?

<blockquote>
The [Restrictionist] Challenge, in a nutshell, is that the evidence of the [human philosophical instrument]’s susceptibility to error makes live the hypothesis that the cathedra lacks resources adequate to the requirements of philosophical enquiry. [[@weinberg2017negative]]{.attribution}
</blockquote>

We're mostly going to bracket metaethical concerns here and assume that moral propositions with relatively stable truth-like values are possible and desirable and that our apprehension of these proposition should satisfy certain properties.

Given that, the overall statement of the [Unreliability of Moral Judgment Problem]{#umjp} looks like this:

1. Ethical and metaethical arguments suggest that certain factors are not relevant for the truth of certain moral claims and ought not to be considered when making moral judgments.
2. Empirical investigation and theoretical arguments suggests that the moral judgments of some people, in some cases, track these morally irrelevant factors.
3. Therefore, for some people, in some cases, moral judgments track factors they ought not to.

Of course, how worrisome that conclusion is depends on how we interpret the "some"s. We'll address that in the [final section](#responses). Before that, we'll look at the second premise. What is the evidence of unreliability?

# Direct (empirical) evidence

We now turn to the central question: "Are our moral intuitions reliable?". There's a fairly broad set of experimental studies examining this question. 

(When we examine each of the putatively irrelevant moral factors below, [for the sake of brevity]{.noted}[^brevity], I'll assume it's obvious why there's at least a *prima facie* case for irrelevance.)

[^brevity]: This was originally written in more innocent times before the post had sprawled to more than 12,000 words.

## Procedure

I attempted a systematic review of these studies. My search procedure was as follows:

1. I searched for "experimental philosophy" and "moral psychology" on [Library Genesis](http://gen.lib.rus.ec/) and selected all books with relevant titles. If I was in doubt based on the title alone, I looked at the book's brief description on Library Genesis.
2. I then examined the table of contents for each of these books and read the relevant chapters. If I was in doubt as to the relevance of a chapter, I read its introduction or did a quick skim.
3. I searched for "reliability of moral intuitions" on [Google Scholar](https://scholar.google.com/) and selected relevant papers based on their titles and abstracts.
4. I browsed the "experimental philosophy: ethics" section of [PhilPapers](https://philpapers.org/browse/experimental-philosophy-ethics/) and selected relevant papers based on their titles and abstracts.
5. Any relevant paper (as judge by title and abstract) that was cited in the works gathered in steps 1-4 was also selected for review.

When selecting works, I was looking for experiments that examined how moral (not epistemological---another common subject of experiment) intuitions about the rightness or wrongness of behavior covaried with factors that are *prima facie* morally irrelevant. I was open to any sort of subject population though most studies ended up examining [WEIRD](https://en.wikipedia.org/wiki/Psychology#WEIRD) college students or workers on online survey platforms like [Amazon Mechanical Turk](https://www.mturk.com/).

I excluded experiments that examined other moral intuitions like:

- whether moral claims are relative
- if moral behavior is intentional or unintentional [@knobe2003intentional]

There were also several studies that examined people's responses to Kahneman and Tversky's [Asian disease scenario](https://en.wikipedia.org/wiki/Framing_(social_sciences)#Experimental_demonstration). Even though this scenario has a strong moral dimension, I excluded these studies on the grounds that any strangeness here was most likely (as judged by my intuitions) a result of non-normative issues (i.e. failure to actually calculate or consider the full implications of the scenario).

For each included study, I extracted information like sample size and the authors' statistical analysis. Some putatively irrelevant factors---order and disgust---had enough studies that homogenizing and comparing the data seemed fruitful. In these cases, I computed the $\eta^2$ effect size for each data point (The code for these calculations can be found [here](https://github.com/colehaus/moral-intuitions-analysis)).

$\eta^2$ is a measure of effect size like the more popular (I think?) [Cohen's $d$](https://en.wikipedia.org/wiki/Effect_size#Cohen's_d). However, instead of measuring the standardized difference of the mean of two populations (like $d$), $\eta^2$ measures the fraction of variation explained. That means $\eta^2$ is just like $R^2$. The somewhat arbitrary conventional classification is that $\eta^2 < 0.05$ represents a small effect, $0.05 \leq \eta^2 < 0.125$ represents a medium effect and anything larger counts as a large effect.

For the factors with high coverage---order and disgust---I also created funnel plots. A [funnel plot](https://en.wikipedia.org/wiki/Funnel_plot) is a way to assess publication bias. If everything is on the up and up, the plot should look like an upside down funnel---effect sizes should spread out symmetrically as we move down from large sample studies to small sample studies. If researchers only publish their most positive results, we expect the funnel to be very lopsided and for the effect size estimated in the largest study to be the smallest.

## Order

Generally, the manipulation in these studies is to present vignettes in different sequences to investigate whether earlier vignettes influence moral intuitions on later vignettes. For example, if a subject receives:

1. a trolley problem where saving five people requires killing one by flipping a switch, and
2. a trolley problem where saving five people requires pushing one person with a heavy backpack into the path of the trolley,

do samples give the same responses to these vignettes regardless of the order they're encountered?

The findings seems to be roughly that:

1. there are some vignettes which elicit stable moral intuitions and are not susceptible to order effects
2. more marginal scenarios are affected by order of presentation
3. the order effect seems to operate like a ratchet in which people are willing to make subsequent judgments stricter but not laxer

But should we actually trust the studies? I give brief comments on the methodology of each study in the [appendix](#order-1). Overall, these studies seemed of pretty methodologically standard to me---no major red flags.

The quantitive results follow. The summary is that while there's substantial variation in effect size and some publication bias, I'm inclined to believe there's a real effect here.

<figure class="big-fig">
<figcaption>Studies of moral intuitions and order effects</figcaption>
| Study                                                   | Independent variable               | Dependent variable                | Sample size                       | Result                                   | Effect size       |
| :------                                                 | :------                            | :------                           | :----                             | :------                                  | ----:             |
| [@petrinovich1996influence], study 2, form 1            | Ordering of inaction vs action     | Scale of agreement                | 30 vs 29                          | $F(1, 57) = 0.37$; $p > 0.10$            | $\eta^2 = 0.0064$ |
| [@petrinovich1996influence], study 2, form 2            | Ordering of inaction vs action     | Scale of agreement                | 30 vs 29                          | $F(1, 57) = 5.07$; $p < 0.02$            | $\eta^2 = 0.080$  |
| [@haidt1996social], mazda                               | Ordering of act vs omission        | Rating act worse                  | [45.5 vs 45.5]{.noted}[^estimate] | $\chi^2 = 7.32$; $p < 0.01$              | $\eta^2 = 0.080$  |
| [@haidt1996social], crane                               | Ordering of act vs omission        | Rating act worse                  | 34.5 vs 34.5                      | $\chi^2 = 0.50$; $p = 0.4795$            | $\eta^2 = 0.007$  |
| [@haidt1996social], mazda                               | Ordering of social roles           | Rating friend worse               | 45.5 vs 45.5                      | $\chi^2 = 3.25$; $p < 0.05$              | $\eta^2 = 0.036$  |
| [@haidt1996social], crane                               | Ordering of social roles           | Rating foreman worse              | 34.5 vs 34.5                      | $\chi^2 = 3.91$; $p < 0.05$              | $\eta^2 = 0.042$  |
| [@lanteri2008experimental]                              | Ordering of vignettes              | Obligatory or not                 | 31 vs 31                          | $\chi^2(1, 62) = 15.17$; $p =  0.000098$ | $\eta^2 = 0.24$   |
| [@lanteri2008experimental]                              | Ordering of vignettes              | Acceptable or not                 | 31 vs 31                          | $\chi^2(1, 62) = 10.63$; $ p=0.0011$     | $\eta^2 = 0.17$   |
| [@lombrozo2009role]                                     | Ordering of trolley switch vs push | Rating of permissibility          | 56 vs 56                          | $t(110) = 3.30$; $p < 0.01$              | $\eta^2 = 0.090$  |
| [@zamzow2009variations]                                 | Ordering of vignettes              | Right or wrong                    | 8 vs 9                            | $\chi^2(1, 17) = 2.837$; $p = 0.09$      | $\eta^2 = 0.17$   |
| [@wright2010intuitional], study 2                       | Ordering of vignettes              | Right or wrong                    | 30 vs 30                          | $\chi^2(1, 60) = 3.2$; $p = 0.073$       | $\eta^2 = 0.053$  |
| [@schwitzgebel2012expertise], philosphers               | Within-pair vignette orderings     | Number of pairs judged equivalent | 324                               | $r = 0.29$; $p < 0.001$                  | $\eta^2 = 0.084$  |
| [@schwitzgebel2012expertise], academic non-philosophers | Within-pair vignette orderings     | Number of pairs judged equivalent | 753                               | $r = 0.19$; $p < 0.001$                  | $\eta^2 = 0.036$  |
| [@schwitzgebel2012expertise], non-academics             | Within-pair vignette orderings     | Number of pairs judged equivalent | 1389                              | $r = 0.21$; $p < 0.001$                  | $\eta^2 = 0.044$  |
| [@liao2012putting]                                      | Ordering of vignettes              | Rating of permissibility          | 48.3 vs 48.3 vs 48.3              | $F(1, 130) = 4.85$; $p < 0.029$          | $\eta^2 = 0.036$  |
| [@wiegmann2012order]                                    | Most vs least agreeable first      | Rating of shouldness              | 25 vs 25                          | $F(1 48) = 8.03$; $p < 0.01$             | $\eta^2 = 0.14$   |
</figure>

[^estimate]: The half is an estimate because 91 subjects were described as being evenly split between the conditions.

<figure>
![Forest plot of order studies](/images/moral-intuitions/order-forest.png)
<figcaption>(Pseudo-)Forest plot showing reported effect sizes for order manipulations</figcaption>
</figure>

While there's clearly dispersion here, that's to be expected given [the heterogeneity of the studies. The most important source of which (I'd guess) is the vignettes used]{.noted}[^no-summary]. The more difficult the dilemma, the more I'd expect order effects to matter and I'd expect some vignettes to show no order effect. I'm not going to endorse murder for fun no matter which vignette you precede it with. Given all this, a study could presumably drive the effect size from ordering arbitrarily low with the appropriate choice of vignettes. On the other hand, it seems like there probably is some upper bound on the magnitude of order effects and more careful studies and reviews could perhaps tease that out.

[^no-summary]: This heterogeneity is also why I don't compute a final, summary measure of the effect size.

<figure>
![Funnel plot of order studies](/images/moral-intuitions/order-funnel.png)
<figcaption>Funnel plot showing reported effect sizes for order manipulations</figcaption>
</figure>

The funnel plot seems to indicate some publication bias, but it looks like the effect may be real even after accounting for that.

## Wording

Unfortunately, I only found one paper directly testing this. In this study, half the participants had their trolley problem described with:

<blockquote>
&#40;a) "Throw the switch, which will result in the death of the one innocent person on the side track" and (b) "Do nothing, which will result in the death of the five innocent people."
</blockquote>

and the other half had their problem described with:

<blockquote>
&#40;a) "Throw the switch, which will result in the five innocent people on the main track being saved" and (b) "Do nothing, which will result in the one innocent person being saved."
</blockquote>
.

The actual consequences of each action are the same in each condition---it's only the wording which has changed. The study (with each of two independent samples) found that indeed people's moral intuitions varied based on the wording:

<figure class="big-fig">
<figcaption>Studies of moral intuitions and wording effects</figcaption>
| Study                                      | Independent variable | Dependent variable | Sample size | Result                               | Effect size       |
| :------                                    | :------              | :------            |     :------ | :------                              | ------:           |
| [@petrinovich1993empirical], general class | Wording of vignettes | Scale of agreement |         361 | $F(1, 359) = 296.51$; $p < 0.000001$ | $\eta_p^2 = 0.45$ |
| [@petrinovich1993empirical], biomeds       | Wording of vignettes | Scale of agreement |          60 | $F(1, 57) = 18.07$; $p = 0.000080$   | $\eta_p^2 = 0.24$ |
</figure>

While the effects are quite large here, it's worth noting that in other studies in other domains framing effects have disappeared when problems were more fully described [@kuhberger1995framing]. [@kuhn1997communicating] found that even wordings which were plausibly equivalent led subjects to alter their estimates of implicit probabilities in vignettes.

## Disgust and cleanliness

In studies of [disgust]{.noted}[^disgust-clean], subjects are manipulated to feel disgust via mechanisms like:

1. recalling and vividly writing about a disgusting experience,
2. watching a clip from [Trainspotting](https://en.wikipedia.org/wiki/Trainspotting_(film)),
3. being exposed to a fart spray, and
4. being hypnotized to feel disgust at the word "often" (Yes, this is really one of the studies).

[^disgust-clean]: There are some studies examining only disgust and some examining only cleanliness, but I've grouped the two here since these manipulations are conceptually related and many authors have examined both.

In studies of cleanliness, subjects are manipulated to feel clean via mechanisms like:

1. doing sentence unscrambling tasks with words about cleanliness,
2. washing their hands, and
3. being in a room where Windex was sprayed.

After disgust or cleanliness is induced (in the non-control subjects), subjects are asked to undertake some morally-loaded activity (usually making moral judgments about vignettes). The hypothesis is that their responses will be different because talk of moral purity and disgust is not merely metaphorical---feelings of cleanliness and incidental disgust at the time of evaluation have a causal effect on moral evaluations. Confusingly, the exact nature of this putative relationship seems rather protean: it depends subtly on whether the subject or the object of a judgment feels clean or disgusted and can be mediated by private body consciousness and response effort.

As the above paragraph may suggest, I'm pretty skeptical of a shocking fraction of these studies (as discussed in more detail in the [appendix](#disgust-and-cleanliness-1)). Some recurring reasons:

1. the manipulations often seem quite weak (e.g. sentence unscrambling, a spritz of Lysol on the questionnaire),
2. the manipulation checks often fail but the authors never seem particularly troubled by this or the fact that they find their predicted results despite the apparent failure of manipulation,
3. authors seem more inclined to explain noisy or apparently contradictory results by complicating their theory than by falsifying their theory, and
4. multiple direct replications have failed.

The quantitative results follow. I'll summarize them in advance by drawing attention to the misshapen funnel plot which I take as strong support for my methodological skepticism. The evidence marshaled so far does not seem to support the claim that disgust and cleanliness influence moral judgments.

<figure class="big-fig">
<figcaption>Studies of moral intuitions and disgust or cleanliness effects</figcaption>
| Study                                  | Independent variable                    | Dependent variable                | Sample size             | Result                                        | Effect size          |
| :------                                | :------                                 | :------                           | :------                 | :------                                       | ------:              |
| [@wheatley2005hypnotic], experiment 1  | Hypnotic disgust cue                    | Scale of wrongness                | 45                      | $t(44) = 2.41$; $p < 0.05$                    | $\eta^2 = 0.12$      |
| [@wheatley2005hypnotic], experiment 2  | Hypnotic disgust cue                    | Scale of wrongness                | 63                      | $t(62) = 1.74$; $p < 0.05$                    | $\eta^2 = 0.073$     |
| [@schnall2008clean], experiment 1      | Clean word scramble                     | Scale of wrongness                | 20 vs 20                | $f(1, 38) = 3.63$; $p = 0.064$                | $\eta^2 = 0.09$      |
| [@schnall2008clean], experiment 2      | Disgusting movie clip                   | Scale of wrongness                | 22 vs 22                | $f(1, 41) = 7.81$; $p = 0.0079$               | $\eta^2 = 0.16$      |
| [@schnall2008disgust], experiment 1    | Fart spray                              | Likert scale                      | 42.3 vs 42.3 vs 42.3    | $f(2, 117) = 7.43$; $p < 0.001$               | $\eta^2 = 0.11$      |
| [@schnall2008disgust], experiment 2    | Disgusting room                         | Scale of appropriacy              | 22.5 vs 22.5            | [Not significant]{.noted}[^info]              |                      |
| [@schnall2008disgust], experiment 3    | Describe disgusting memory              | Scale of appropriacy              | 33.5 vs 33.5            | Not significant                               |                      |
| [@schnall2008disgust], experiment 4    | Disgusting vs sad vs neutral movie clip | Scale of appropriacy              | 43.3 vs 43.3 vs 43.3    | $f(1, 104) = 4.11$; $p < 0.05$                | $\eta^2 = 0.038$     |
| [@horberg2009disgust], study 2         | Disgusting vs sad movie clip            | Scale of rightness and wrongness  | 59 vs 63                | $F(1, 115) = 4.51$; $p < 0.01$                | $\eta^2 = 0.038$     |
| [@liljenquist2010smell], experiment 1  | Clean scent in room                     | Money returned                    | 14 vs 14                | $t(26) = 2.64$; $p = 0.01$                    | $\eta^2 = 0.21$      |
| [@liljenquist2010smell], experiment 2  | Clean scent in room                     | Scale of volunteering interesting | 49.5 vs 49.5            | $t(97) = 2.33$; $p = 0.02$                    | $\eta^2 = 0.052$     |
| [@liljenquist2010smell], experiment 2  | Clean scent in room                     | Willingness to donate             | 49.5 vs 49.5            | $\chi^2(1, 99) = 4.78$; $p = 0.03$            | $\eta^2 = 0.048$     |
| [@zhong2010clean], experiment 1        | Antiseptic wipe for hands               | Scale of immoral to moral         | 29 vs 29                | $t(56) = 2.10$; $p = 0.04$                    | $\eta^2 = 0.073$     |
| [@zhong2010clean], experiment 2        | Visualize clean vs dirty and nothing    | Scale of immoral to moral         | 107.6 vs 107.6 vs 107.6 | $t(320) = 2.02$; $p = 0.045$                  | $\eta^2 = 0.013$     |
| [@zhong2010clean], experiment 2        | Visualize dirty vs nothing              | Scale of immoral to moral         | 107.6 vs 107.6 vs 107.6 | $t(320) = 0.42$; $p = 0.675$                  | $\eta^2 = 0.00055$   |
| [@zhong2010clean], experiment 3        | Visualize clean vs dirty                | Scale of immoral to moral         | 68 vs 68                | $t(134) = 2.13$; $p = 0.04$                   | $\eta^2 = 0.033$     |
| [@eskine2011bad]                       | Sweet, bitter or neutral drink          | Scale of wrongness                | 18 vs 15 vs 21          | $F(2, 51) = 7.368$; $p = 0.002$               | $\eta^2 = 0.224$     |
| [@david2011effect]                     | Presence of disgust-conditioned word    | Scale of wrongness                | 61                      | $t(60) = 0.62$; Not significant               | $\eta^2 = 0.0064$    |
| [@tobia2013cleanliness], undergrads    | Clean scent on survey                   | Scale of wrongness                | 84 vs 84                | $f(1, 164) = 8.56$; $p = 0.004$               | $\eta^2 = 0.05$      |
| [@tobia2013cleanliness], philosophers  | Clean scent on survey                   | Scale of wrongness                | 58.5 vs 58.5            | Not significant                               |                      |
| [@huang2014does], study 1              | Clean word scramble                     | Scale of wrongness                | 111 vs 103              | $t(212) = -1.22$; $p = 0.23$                  | $\eta^2 = 0.0072$    |
| [@huang2014does], study 2              | Clean word scramble                     | Scale of wrongness                | 211 vs 229              | $t(438) = -0.42$; $p = 0.68$                  | $\eta^2 = 0.0040$    |
| [@johnson2014does], experiment 1       | Clean word scramble                     | Scale of wrongness                | 114.5 vs 114.5          | $f(1, 206) = 0.004$; $p = 0.95$               | $\eta^2 = 0.000019$  |
| [@johnson2014does], experiment 2       | Washing hands                           | Scale of wrongness                | 58 vs 68                | $f(1, 124) = 0.001$; $p = 0.97$               | $\eta^2 = 0.0000081$ |
| [@johnson2016effects], study 1         | Describe disgusting memory              | Scale of wrongness                | 222 vs 256              | $f(1, 474) = 0.04$; $p = 0.84$                | $\eta^2 = 0.000084$  |
| [@johnson2016effects], study 2         | Describe disgusting memory              | Scale of wrongness                | 467 vs 467              | $f(1, 926) = 0.48$; $p = 0.48$                | $\eta^2 = 0.00052$   |
| [[@daubman2014]]{.noted}[^file-drawer] | Clean word scramble                     | Scale of wrongness                | 30 vs 30                | $t(58) = 1.84$; $p = 0.03$                    | $\eta^2 = 0.054$     |
| [@daubman2013]                         | Clean word scramble                     | Scale of wrongness                | 30 vs 30                | [$t(58) = -1.8$]{.noted}[^coding]; $p = 0.04$ | $\eta^2 = 0.053$     |
| [@johnson2014]                         | Clean word scramble                     | Scale of wrongness                | 365.6 vs 365.5          | $F(1, 729) = 0.31$; $p = 0.58$                | $\eta^2 = 0.00043$   |
</figure>

[^file-drawer]: The last three results are [file drawer](https://en.wikipedia.org/wiki/Publication_bias) results from [PsychFileDrawer](https://www.psychfiledrawer.org).
[^coding]: I assume the coding was just flipped somewhere between [@daubman2013] and [@daubman2014] since they have $t(58) = 1.84$ and $t(58) = -1.8$ but declare both to be successful replications.
[^info]: Unfortunately, they didn't report more detailed information.

<figure>
![Forest plot of disgust/cleanliness studies](/images/moral-intuitions/clean-forest.png)
<figcaption>(Pseudo-)Forest plot showing reported effect sizes for disgust/cleanliness manipulations</figcaption>
</figure>

<figure>
![Funnel plot of disgust/cleanliness studies](/images/moral-intuitions/clean-funnel.png)
<figcaption>Funnel plot showing reported effect sizes for disgust/cleanliness manipulations</figcaption>
</figure>

This funnel plot suggests pretty heinous publication bias. I'm inclined to say that the evidence does not support claims of a real effect here.

## Gender

This factor has extra weight within the field of philosophy because it's been offered as an explanation for the relative scarcity of woman in academic philosophy [@buckwalter2014gender]: if women's philosophical intuitions systematically diverge from those of men and from canonical answers to various thought experiments, they may find themselves discouraged.

Studies on this issue typically just send surveys to people with a series of vignettes and analyze how the results vary depending on gender.

I excluded [@buckwalter2014gender] entirely for reasons described in the [appendix](#gender-1).

Here are the quantitative results:

<figure class="big-fig">
<figcaption>Studies of moral intuitions and gender effects</figcaption>
| Study                                                         | Independent variable | Dependent variable                 | Sample size   | Result                         |
| :------                                                       | :------              | :------                            | :------       | :------                        |
| [@lombrozo2009role], trolley switch                           | Gender               | Scale of permissibility            | 74.7 vs 149.3 | $t(222) = -0.10$, $p = 0.92$   |
| [@lombrozo2009role], trolley push                             | Gender               | Scale of permissibility            | 74.7 vs 149.3 | $t(222) = -0.69$, $p = 0.49$   |
| [@seyedsayamdost2015gender], plank of Carneades, MTurk        | Gender               | Scale of blameworthiness           | 70 vs 86      | $t(154) = -1.302$, $p = 0.195$ |
| [@seyedsayamdost2015gender], plank of Carneades, SurveyMonkey | Gender               | Scale of blameworthiness           | 48 vs 50      | $t(96) = 0.727$, $p = 0.469$   |
| [@adleberg2015men], violinist                                 | Gender               | Scale from forbidden to obligatory | 52 vs 84      | $t(134) = -0.39$, $p = 0.70$   |
| [@adleberg2015men], magistrate and the mob                    | Gender               | Scale from bad to good             | 71 vs 87      | $t(156) = -0.28$, $p = 0.78$   |
| [@adleberg2015men], trolley switch                            | Gender               | Scale of acceptability             | 52 vs 84      | $t(134) = 0.26$, $p = 0.34$    |
</figure>

As we can see, there doesn't seem to be good evidence for an effect here.

## Culture and socioeconomic status

There's just [one study here]{.noted}[^ultimatum]. It tested responses to moral vignettes across high and low socioeconomic status samples in Philadelphia, USA and Porto Alegre and Recife, Brazil.

[^ultimatum]: There are quite a few cross-cultural studies of things like the ultimatum game [@henrich2001search]. I excluded those because they are not purely moral---the ultimatum-giver is also trying to predict the behavior of the ultimatum-recipient.

As mentioned in the [appendix](#culture-and-socioeconomic-status-1), I find the seemingly very artificial dichotimization of the outcome measure a bit strange in this study.

Here are the quantitative results:

<figure class="big-fig">
<figcaption>Studies of moral intuitions and culture/SES effects</figcaption>
| Study                        | Independent variable | Dependent variable | Sample size | Result                          |
| :------                      | :------              | :------            | :------     | :------                         |
| [@haidt1993affect], adults   | Culture              | Acceptable or not  | 90 vs 90    | $F(1, 174) = 5.6$; $p < 0.01$   |
| [@haidt1993affect], children | Culture              | Acceptable or not  | 90 vs 90    | $F(1, 174) = 5.91$; $p < 0.01$  |
| [@haidt1993affect], adults   | SES                  | Acceptable or not  | 90 vs 90    | $F(1, 174) = 73.1$; $p < 0.001$ |
| [@haidt1993affect], children | SES                  | Acceptable or not  | 90 vs 90    | $F(1, 174) = 9.00$; $p < 0.01$  |
</figure>

The study found that Americans and those of high socioeconomic status were more likely to judge disgusting but harmless activities as morally acceptable.

## Personality

There's just one survey here examining how responses to vignettes varied with [Big Five personality](https://en.wikipedia.org/wiki/Big_Five_personality_traits) traits.

<figure class="big-fig">
<figcaption>Studies of moral intuitions and personality effects</figcaption>
| Study                                | Independent variable | Dependent variable     | Sample size | Result                       |
| :------                              | :------              | :------                | :------     | :------                      |
| [@feltz2008fragmented], experiment 2 | Extraversion         | Is it wrong? Yes or no | 162         | $r(146) = 0.23$, $p = 0.005$ |
</figure>

## Actor/observer

In these studies, one version of the vignette has some stranger as the central figure in the dilemma. The other version puts the survey's subject in the moral dilemma. For example, "Should Bob throw the trolley switch?" versus "Should you throw the trolley switch?".

I'm actually mildly skeptical that inconsistency here is necessarily anything to disapprove of. Subjects know more about themselves than about arbitrary characters in vignettes. That extra information could be justifiable grounds for different evaluations. For example, if subjects understand themselves to be more likely than the average person to be haunted by utilitarian sacrifices, that could ground different decisions in moral dilemmas calling for utilitarian sacrifice.

Nevertheless, the quantitative results follow. They generally find there is a significant effect.

<figure class="big-fig">
<figcaption>Studies of moral intuitions and actor/observer effects</figcaption>
| Study                                                | Independent variable | Dependent variable             | Sample size  | Result                                                                                     |
| :------                                              | :------              | :------                        | :------      | :------                                                                                    |
| [@nadelhoffer2008actor], trolley switch, undergrads  | Actor vs observer    | Morally permissible? Yes or no | 43 vs 42     | 90% permissible in observer condition; 65% permissible in actor condition; $p = 0.029$     |
| [@tobia2013moral], trolley switch, philosophers      | Actor vs observer    | Morally permissible? Yes or no | 24.5 vs 24.5 | 64% permissible in observer condition; 89% permissible in actor condition; $p < 0.05$      |
| [@tobia2013moral], Jim and the natives, undergrads   | Actor vs observer    | Morally obligated? Yes or no   | 20 vs 20     | 53% obligatory in observer condition; 19% obligatory in actor condition; $p < 0.05$        |
| [@tobia2013moral], Jim and the natives, philosophers | Actor vs observer    | Morally obligated? Yes or no   | 31 vs 31     | 9% obligatory in the observer condition; 36% obligatory in the actor condition; $p < 0.05$ |
| [@tobia2013cleanliness], undergrads                  | Actor vs observer    | Scale of wrongness             | 84 vs 84     | $f(1, 164) = 15.24$; $p < 0.0001$                                                          |
| [@tobia2013cleanliness], philosophers                | Actor vs observer    | Scale of wrongness             | 58.5 vs 58.5 | Not significant                                                                            |
</figure>

## Summary

Order
:   Lots of studies, overall there seems to be evidence of an effect
Wording
:   Just one study, big effect, strikes me as plausible that there's an effect here
Disgust and cleanliness
:   Lots of studies, lots of methodological problems and lots of publication bias, I round this to no good evidence of the effect
Gender
:   Medium amount of studies, studies generally don't find evidence of an effect
Culture and socioeconomic status
:   One study, found effect, seems hard to imagine there's no effect here
Personality
:   One study, found effect
Actor/observer
:   A couple of studies, found big effects, strikes me as plausible that there's an effect here

# Indirect evidence

Given that the direct evidence isn't quite definitive, it may be useful to look at some indirect evidence. By that, I mean we'll look at (among other things) some underlying theories about how moral intuitions operate and what bearing they have on the question of reliability.

## Heuristics and biases

No complex human faculty is perfectly reliable. This is no surprise and perhaps not of great import.

But we have evidence that some faculties are not only "not perfect" but systematically and substantially biased. The heuristics and biases program (heavily associated with Kahneman and Tversky) of research has [shown]{.noted}[^caveat] serious limitations in human rationality. A review of that literature is out of scope here, but the list of alleged aberrations is extensive. [Scope insensitivity](https://en.wikipedia.org/wiki/Scope_neglect)---the failure of people, for example, to care twice as much about twice as many oil-covered seagulls---is one example I find compelling.

[^caveat]: Yes, [not all results](https://replicationindex.com/2017/02/02/reconstruction-of-a-train-wreck-how-priming-research-went-of-the-rails/) in works like [Thinking Fast and Slow](https://en.wikipedia.org/wiki/Thinking,_Fast_and_Slow) have held up and some of the results are in areas prone to replication issues. It still seems unlikely that *all* such results will be swept away and we'll be left to conclude that humans were perfectly rational all along.

How relevant these problems are for moral judgment is a matter of some interpretation. An argument for relevance is this: even supposing we have *sui generis* moral faculties for judging purely normative claims, much day-to-day "moral" reasoning is actually prudential reasoning about how best to achieve our ends given constraints. This sort of prudential reasoning is squarely in the crosshairs of the heuristics and biases program.

<!-- How relevant these problems are depends on how tight you think the analogy is. On one point of view, there's little overlap. Instrumental rationality as studied in the heuristics and biases program is purely about means---given a goal, how is it best achieved---while morality is about ends---what should our goals be. The other point of view (which I find more persuasive) suggests that there's significant overlap between the faculties. Most day-to-day moral judgments don't call for us to reconsider our ultimate ends. Instead, these judgments are about what actions we should take in the moment in service of our ultimate ends. Judgments of that sort sound much closer to the means-based domain of rationality. From this perspective, there may not even be a clear division between instrumental rationality and moral judgments about which actions to perform---merely a gradient of how salient prudential concerns are versus moral concerns.  -->

At a minimum, prominent heuristics and biases researcher Gerd Gigerenzer endorses the hypothesis that heuristics underlying moral behavior are "largely" the same as heuristics underlying other behavior [@gigerenzer2008moral]. He explains, "Moral intuitions fit the pattern of heuristics, in our "narrow" sense, if they involve (a) a target attribute that is relatively inaccessible, (b) a heuristic attribute that is more easily accessible, and (c) an unconscious substitution of the target attribute for the heuristic attribute." Condition (a) is satisfied by many accounts of morality and heuristic attributes as mentioned in (b) abound (e.g. how bad does it feel to think about action A). It seems unlikely that the substitution described in (c) fails to happen only in the domain of moral judgments.

## Neural

Now we'll look at unreliability at a lower level.

A distinction is sometimes drawn between joint evaluations---choice---and single evaluations---judgment. In a choice scenario, an actor has to choose between multiple options presented to them simultaneously. For example, picking a box of cereal in the grocery store requires choice. In a judgment scenario, an actor makes some evaluation of an option presented in isolation. For example, deciding how much to pay for a used car is judgment scenario.

For both tasks, leading models are (as far as I understand things) fundamentally stochastic.

Judgment tasks are described by the random utility model in which, upon introspection, an actor samples from a distribution of possible valuations for an option rather than finding a single, fixed valuation [@glimcher2005physiological]. This makes sense at the neuronal level because liking is encoded as the firing rate of a neuron and firing rates are stochastic.

Choice tasks are described by the drift diffusion model in which the current disposition to act starts at 0 on some axis and takes a biased random walk (drifts) [@ratcliff2008diffusion]. Away from zero, on two opposite sides, are thresholds representing each of the two options. Once the current disposition drifts past a threshold, the corresponding option is chosen. Because of the random noise in the drift process, there's no guarantee that the threshold favored by the bias will always be the first one crossed. Again, the randomness in this model makes sense because neurons are stochastic.

<figure>
![Plot showing drift diffusion model](/images/moral-intuitions/drift-diffusion.png)
<figcaption>Example of ten evidence accumulation sequences for the drift diffusion model, where the true result is assigned to the upper threshold. Due to the addition of noise, two sequences have produced an inaccurate decision. From [Wikipedia](https://en.wikipedia.org/wiki/Two-alternative_forced_choice).</figcaption>
</figure>

So for both choice and judgment tasks, low-level models and neural considerations suggest that we should expect noise rather than perfectly reliability. And we should probably expect this to apply equally in the moral domain. Indeed, experimental evidence suggests that a drift diffusion model can be fit to moral decisions [@crockett2014harm] [@hutcherson2015neurocomputational].

## Dual process

Josh Greene's dual process theory of moral intuitions [@greene2007vmpfc] suggests that we have two different types of moral intuitions originating from two different cognitive systems. System 1 is emotional, automatic and produces characteristically deontological judgments. System 2 is non-emotional, reflective and produces characteristically consequentialist judgments.

He makes the further claim that these deontological, system 1 judgments ought not to be trusted in novel situations because their automaticity means they fail to take new circumstances into account.

## Genes

All complex behavioral traits have substantial genetic influence [@plomin2016top]. Naturally, moral judgments are part of "all". This means certain traits relevant for moral judgment are evolved. But an evolved trait is not necessarily an adaptation. A trait only rises to the level of adaptation if it was the result of natural selection (as opposed to, for example, random drift).

If our evolved faculties for moral judgment are not adaptations (i.e. they are random and not the product of selection), it seems clear that they're unlikely to be reliable.

On the other hand, might adaptations be reliable? Alas, even if our moral intuitions are adaptive [this is no guarantee that they track the truth]{#unreliable-evolution}. First, knowledge is not always fitness relevant. For example, "perceiving gravity as a distortion of space-time" would have been no help in the ancestral environment [@krasnow2017evolutionarily]. Second, asymmetric costs and benefits for false positives and false negatives means that perfect calibration isn't necessarily optimal. Prematurely condemning a potential hunting partner as untrustworthy comes at minimal cost if there are other potential partners around while getting literally stabbed in the back during a hunt would be very costly indeed. Finally, because we are socially embedded, wrong beliefs can increase fitness if they affect how others treat us.

Even if our moral intuitions are adaptations and were reliable in the ancestral environment, that's no guarantee that they're reliable in the modern world. There's reason to believe that our moral intuitions are not well-tuned to "evolutionarily novel moral dilemmas that involve isolated, hypothetical, behavioral acts by unknown strangers who cannot be rewarded or punished through any normal social primate channels". [@miller2007sexual] (Though for a contrary point of view about social conditions in the ancestral environment, see [@turner2013evolution].) This claim is especially persuasive if we believe that (at least some of) [our moral intuitions are the result of a fundamentally reactive, retrospective process like Greene's system 1]{.noted}[^shift].

[^shift]: We can also phrase this as follows: Some of our moral intuitions are the result of model-free reinforcement learning [@crockett2013models]. In the absence of a model specifying action-outcome links, these moral intuitions are necessarily retrospective. Framed in this ML way, the concern is that our moral intuitions are not robust to distributional shift [@amodei2016concrete].

::: {.skippable}
If you're still skeptical about the role of biological evolution in our faculties for moral judgment, Tooby and Cosmides's social contract theory is often taken to be strong evidence for the evolution of some specifically moral faculties. Tooby and Cosmides are advocates of the [massive modularity](https://en.wikipedia.org/wiki/Modularity_of_mind#Evolutionary_psychology_and_massive_modularity) thesis according to which the human brain is composed of a large number of special purpose modules each performing a specific computational task. Social contract theory finds that people are much better at detecting violations of conditional rules when those rules encode a social contract. [Tooby and Cosmides]{.noted}[^trash] take this to mean that we have evolved a special-purpose module for analyzing obligation in social exchange which cannot be applied to conditional rules in the general case.
:::

[^trash]: Aside: There is some amazing academic trash talk in chapter 2 of [@sinnott2008moral]. Just utter contempt dripping from every paragraph on both sides (Jerry Fodor versus Tooby and Cosmides). For example, "Those familiar with Fodor’s writing know that he usually resurrects his grandmother when he wants his intuition to do the work that a good computational theory should.".

(There's a lot more research on the deep roots of cooperation and morality in humans:  [@boyd2005origin], [@boyd2003evolution], [@hauert2007via], [@singer2000darwinian].)

### Universal moral grammar

Linguists have observed a [poverty of the stimulus](https://en.wikipedia.org/wiki/Poverty_of_the_stimulus)---children learn how to speak a natural language without anywhere near enough language experience to precisely specify all the details of that language. The solution that Noam Chomsky came up with is a [universal grammar](https://en.wikipedia.org/wiki/Universal_grammar)---humans have certain language rules hard-coded in our brains and language experience only has to be rich enough to select among these, not construct them entirely.

Researchers have made similar claims about morality [@sripada2008nativism]. The argument is that children learn moral rules without enough moral experience to precisely specify all the details of those rules. Therefore, they must have a universal moral grammar---innate faculties that encode certain possible moral rules. There are of course arguments against this claim. Briefly: moral rules are much less complex than languages, and (some) language learning must be inductive while moral learning can include explicit instruction.

If our hard-coded moral rules preclude us from learning the true moral rules (a possibility on some metaethical views), our moral judgments would be very unreliable indeed [@millhouse2018learnability].

## Culture

I'll take it as fairly obvious that our moral judgments are [culturally influenced]{.noted}[^blurry] (see e.g. [@henrich2004foundations]). A common story for the role of culture in moral judgments and behavior is that norms of conditional cooperation arose to solve cooperation problems inherent in group living [@curry2016morality] [@hechter2001social]. But, just [as we discussed with biological evolution](#unreliable-evolution), these selective pressures aren't necessarily aligned with the truth.

[^blurry]: The separation between [culture](#culture) and [genes](#genes) is particularly unclear when looking at norms and moral judgment since both culture and genes are plausibly working to solve (at least some of) the same problems of social cooperation. One synthesis is to suppose that certain faculties eventually evolved to facilitate some culturally-originated norms.

One of the alternative accounts of moral judgments as a product of culture is the social intuitionism of Haidt and Bjorklund [@haidt2008social]. They argue that, at the individual level, moral reasoning is usually a post-hoc confabulation intended to support automatic, intuitive judgments. Despite this, these confabulations have causal power when passed between people and in society at large. These socially-endorsed confabulations accumulate and eventually become the basis for our private, intuitive judgments. Within this model, it seems quite hard to arrive at the conclusion that our moral judgments are highly reliable.

## Moral disagreements

There's quite a bit of literature on the implications of enduring [moral disagreement](https://plato.stanford.edu/entries/moral-realism/#1). I'll just briefly mention that, on many metaethical views, it's not trivial to reconcile perfectly reliable moral judgments and enduring moral disagreement. (While I think this is an important line of argument: I'm giving it short shrift here because: 1. the fact of moral disagreement is no revelation, and 2. it's hard to make it bite---it's too easy to say, "Well, we disagree because I'm right and they're wrong.".)

## Summary

Heuristics and biases
:   There's lots of evidence that humans are not instrumentally rational. This probably applies at least somewhat to moral judgments too since prudential reasoning is common in day-to-day moral judgments.
Neural
:   Common models of both choice and judgment are fundamentally stochastic which reflects the stochasticity of neurons.
Dual process
:   System 1 moral intuitions are automatic, retrospective and untrustworthy.
Genes
:   Moral faculties are at least partly evolved. Adaptations aren't necessarily truth-tracking---especially when removed from the ancestral environment.
Culture
:   Culture influences moral judgment and cultural forces don't necessarily incentivize truth-tracking.
Moral disagreement
:   There's a lot of disagreement about what's moral and it's hard to both accept this and claim that moral judgments are perfectly reliable.

<!-- The main takeaway here is that there's lots of discussion about moral judgments---how they originate, how they operate, what function they serve. Some of these theories are complementary and some compete with each other. But all of them give us reason to seriously doubt the reliability of moral judgments. -->

# Responses

Depending on how skeptical of skepticism you're feeling, all of the above might add up to serious doubts about the reliability of our moral intuitions. How might we respond to these doubts? There are a variety of approaches discussed in the literature. I will group these responses loosely based on how they fit into the structure of the [Unreliability of Moral Judgment Problem](#umjp):

- The first type of response simply questions the internal validity of the empirical studies calling the core of premise 2 into question.
- The second type of response questions the external validity of the studies thereby asserting that the "some"s in premise 2 ("some people, in some cases") are narrow enough to defuse any real threat in the conclusion.
- The third type of response accepts the whole argument and argues that it's not too worrisome.
- The fourth type of response accepts the whole argument and argues that we can take countermeasures.

## Internal validity

If the experimental results that purport to show that moral judgments are unreliable lack internal validity, the argument as a whole lacks force. On the other hand, the invalidity of these studies isn't affirmative evidence that moral judgments are reliable and the indirect evidence may still be worrying.

The validity of the studies is discussed in the [direct evidence section](#direct-empirical-evidence) and in the [appendix](#appendix-qualitative-discussion-of-methodology) so [I won't repeat it here]{.noted}[^categorical]. I'll summarize my take as: the cleanliness/disgust studies have low validity, but the order studies seem plausible and I believe that there's a real effect there, at least on the margin. Most of the other factors don't have enough high-quality studies to draw even a tentative conclusion. Nevertheless, when you add in my priors and the indirect evidence, I believe there's reason to be concerned.

[^categorical]: I will add one complaint that applies to pretty much all of the studies: they treat [categorical scale](https://en.wikipedia.org/wiki/Level_of_measurement#Nominal_level) data (e.g. responses on a [Likert scale](https://en.wikipedia.org/wiki/Likert_scale)) as [ratio scale](https://en.wikipedia.org/wiki/Level_of_measurement#Ratio_scale). But this sort of thing seems rampant so isn't a mark of exceptional unreliability in this corner of the literature.

## Expertise

The most popular response among philosophers (surprise, surprise) is the expertise defense: The moral judgments of the folk may track morally irrelevant factors, but philosophers have acquired special expertise which [immunizes them from these failures]{.noted}[^second-order-expertise]. There is an immediate appeal to the argument: What does expertise mean if not increased skill? There is even supporting evidence in the form of trained philosophers' improved performance on cognitive reflection tests (This test asks questions with intuitive but incorrect responses. For example, "A bat and a ball cost $1.10 in total. The bat costs $1.00 more than the ball. How much does the ball cost?". [@frederick2005cognitive]).

[^second-order-expertise]: There's also the slightly subtler claim that expertise does not purify moral intuitions and judgments, but that it helps philosophers understand and accomodate their cognitive flaws [@alexander2016philosophical]. We'll not explicitly examine this claim any further here.

Alas, that's where the good news ends and the trouble begins. As [@weinberg2010philosophers] describes it, the expertise defense seems to rely on a folk theory of expertise in which experience in a domain inevitably improves skill in all areas of that domain. Engagement with the research on expert performance significantly complicates this story.

First, it seems to be the case that not all domains are conducive to the development of expertise. For example, training and experience do not produce expertise at psychiatry and stock brokerage according to [@dawes1994psychotherapy] and [@shanteau1992competence]. Clear, immediate and objective feedback appears necessary for the formation of expertise [@shanteau1992competence]. Unfortunately, it's hard to construe whatever feedback is available to moral philosophers considering thought experiments and edge cases as clear, immediate and objective [@clarke2013intuitions] [@weinberg2007challenge].

Second, "one of the most enduring findings in the study of expertise [is that there is] little transfer from high-level proficiency in one domain to proficiency in other domains---even when the domains seem, intuitively, very similar" [@feltovich2006studies]. Chess experts have excellent recall for board configurations, but only when those configurations could actually arise during the course of a game [@degroot2014]. Surgical expertise carries over very little from one surgical task to another [@norman2006expertise]. Thus, evidence of improved [cognitive reflection is not a strong indicator of improved moral judgment]{.noted}[^reflection]. Nor is evidence of philosophical excellence on any task other than moral judgment itself likely to be particularly compelling. (And even "moral judgment" may be too broad and incoherent a thing to have uniform skill at.)

[^reflection]: There is even reason to believe that reflection is sometimes harmful [@kornblith2010reflective] [@weinberg2014intuitions].

Third, it's not obvious that expertise immunizes from biases. Studies have claimed that Olympic gymnastics judges and professional auditors are vulnerable to order effects despite being expert in other regards [@brown2009order] [@damisch2006olympic].

Finally, there is direct empirical evidence that [philosophers moral judgments continue to track putatively morally irrelevant factors]{.noted}[^moral-philosophers]. See [@tobia2013cleanliness], [@tobia2013moral] and [@schwitzgebel2012expertise] already described above. ([@schulz2011persistent] find similar results for another type of philosophical judgment.)

[^moral-philosophers]: There's also the interesting but somewhat less relevant work of Schwitzgebel and Rust [@schwitzgebel2016behavior] in which they repeatedly find that ethicists do not behave more morally (according to their metrics) than non-ethicists.

So, in sum, while there's an immediate appeal to the expertise defense (surely we can trust intuitions honed by years of philosophical work), it looks quite troubled upon deeper examination.

## Ecological validity

It's always a popular move to speculate that the lab isn't like the real world and so lab results don't apply in the real world. Despite the saucy tone in the preceding sentence, I think there are real concerns here:

- Reading vignettes is not the same as actually experiencing moral dilemmas first-hand.
- Stated judgments are not the same as actual judgments and behaviors. For example, none of the studies mentioned doing anything (beyond standard anonymization) to combat social desirability bias.

However, it's not clear to me how these issues license a belief that real moral judgments are likely to be reliable. One can perhaps hope that we're more reliable when the stakes truly matter, but it would take a more detailed theory for the ecological validity criticisms to have an impact.

## Sufficient

One way of limiting the force of the argument against the reliability of moral judgments is simply to point out that many judgments *are* reliable and immune to manipulation. This is certainly true; order effects are not omnipotent. I'm not going to go out and murder anyone just because you prefaced the proposal with the right vignette.

Another response to the experimental results is to claim that even though people's ratings as measured with a [Likert scale](https://en.wikipedia.org/wiki/Likert_scale) changed, the number of people actually switching from moral approval to disapproval or vice versa (i.e. moving from one half of the Likert scale to the other) is unreported and possibly small [@demaree2016framing].

The first response to this response is that the truth of this claim even as reported in the paper making the argument depends on your definition of "small". I think a 20% probability of switching from moral approval to disapproval based on the ordering of vignettes is not small.

The second set of responses to this attempted defusal is as follows. Even if experiments only found shifts in degree of approval or disapproval, that would be worrying because:

- Real moral decisions often involve trade-offs between two wrongs or two rights and the degree of rightness or wrongness of each component in such dilemmas may determine the final judgment rendered. [@andow2016reliable]
- Much philosophical work involves thought experiments on the margin where approval and disapproval are closely balanced [@wright2016intuitional]. Even small effects from improper sources at the border can lead to the wrong judgment. Wrong judgments on these marginal thought experiments can cascade to more mundane and important judgments if we take them at face value and apply something like [reflective equilibrium](https://plato.stanford.edu/entries/reflective-equilibrium/).

## Ecologically rational

Gerd Gigerenzer likes to make the argument (contra Kahneman and Tversky; some more excellent academic slap fights here) that heuristics are ecologically rational [@todd2012ecological]. By this, he means that they are optimal in a given context. He also talks about less-is-more effects in which [simple heuristics actually outperform more complicated and apparently ideal strategies]{.noted}[^heuristics].

[^heuristics]: Gigerenzer explains this surprising result by appealing to the [bias-variance tradeoff](https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff)---complicated strategies over-fit to the data they happen to see and fail to generalize. Another explanation is that heuristics represent an infinitely strong prior and that the "ideal" procedures Gigerenzer tested against represent an uninformative prior [@parpart2018heuristics].

One could perhaps make an analogous argument for moral judgments: though they don't always conform to the dictates of ideal theory, they are near optimal given the environment in which they operate. Though we can't relitigate the whole argument here, I'll point out that there's lots of pushback against Gigerenzer's view. Another response to the response would be to highlight ways in which moral judgment is unique and the ecological validity response doesn't apply to moral heuristics.

## Second-order reliability

Even if we were to accept that our moral judgments are unreliable, that might not be fatal. If we could judge when our moral judgments are reliable---if we had reliable second-order moral judgments---we could rely upon our moral judgments only in domains where we knew them to be valid. 

Indeed, there's evidence that, in general, we are more confident in our judgments when they turn out to be correct [@gigerenzer1991probabilistic]. But subsequent studies have suggested our confidence actually tracks consensuality rather than correctness [@koriat2008subjective]. People were highly confident when asked about popular myths (for example, whether Sydney is the capital of Australia). This possibility of consensual, confident wrongness is pretty worrying [@williams2015possibility].

Jennifer Wright has two papers examining this possibility empirically. In [@wright2010intuitional], she found that more confident epistemological and ethical judgments were less vulnerable to order effects. Thus, lack of confidence in a philosophical intuition may be a reliable indicator that the intuition is unreliable. [@wright2013tracking] purports to address related questions, but I found I found it unconvincing for a variety of reasons.

## Moral engineering

The final response to evidence of unreliability is to argue that we can overcome our deficiencies by application of careful effort. Engineering reliable systems and processes from unreliable components is a recurring theme in human progress. The physical sciences work with imprecise instruments and overcome that limitation through careful design of procedures and statistical competence. In [distributed computing](https://en.wikipedia.org/wiki/Distributed_computing), we're able to [build reliable systems out of unreliable components](https://en.wikipedia.org/wiki/Consensus_(computer_science)).

As a motivating example, imagine a set of [litmus](https://en.wikipedia.org/wiki/Litmus) strips which turn red in acid and blue in base [@weinberg2016experimental]. Now suppose that each strip has only a 51% chance of performing correctly---red in an acid and blue in base. Even in the face of this radical unreliability, we can drive our confidence to an arbitrarily high level by testing the material with more and more pH strips (as long as each test is independent).

This analogy provides a compelling motivation for coherence norms. By demanding that our moral judgments across cases cohere, we are implicitly aggregating noisy data points into a larger system that we hope is more reliable. It may also motivate an increased deference to an "outside view" which aggregates the moral judgments of many.

[@huemer2008revisionary] presents another constructive response to the problem of unreliable judgments. It proposes that concrete and mid-level intuitions are especially unreliable because they are the most likely to be influenced by culture, biological evolution and emotions. On the other hand, fully abstract intuitions are prone to overgeneralizations in which the full implications of a claim are not adequately understood. If abstract judgments and concrete judgments are to be distrusted, what's left? Huemer proposes that formal rules are unusually trustworthy. By formal rules, he is referring to rules which impose constraints on other rules but do not themselves produce moral judgments. Examples include transitivity (If A is better than B and B is better than C, A must be better than C.) and compositionality (If doing A is wrong and doing B is wrong, doing both A and B must be wrong.).

Other interesting work in this area includes [@weinberg2012intuition], [@weinberg2017knowledge], and [@talbot2014so].

[@weinberg2016experimental] summarizes this perspective well:

<blockquote>
Philosophical theory-selection and empirical model-selection are highly similar problems: in both, we have a data stream in which we expect to find both signal and noise, and we are trying to figure out how best to exploit the former without inadvertently building the latter into our theories or models themselves.
</blockquote>

## Summary

Internal validity
:    The experimental evidence isn't great, but it still seems hard to believe that our moral judgments are perfectly reliable.
Expertise
:    Naive appeals to expertise are unlikely to save us given the literature on expert performance.
Ecological validity
:    The experiments that have been conduct are indeed different from *in vivo* moral judgments, but it's not currently obvious that the move from synthetic to natural moral dilemmas will improve judgment.
Sufficient
:    Even if the effects of putatively irrelevant factors are relatively small, that still seems concerning given that moral decisions often involve complex trade-offs.
Ecologically rational
:    I'm not particularly convinced by Gigerenzer's view of heuristics as ecologically rational and I'm even more inclined to doubt that this is solid ground for moral judgments.
Second-order reliability
:    Our sense of the reliability of our moral judgments probably isn't pure noise. But it's also probably not perfect.
Moral engineering
:    Acknowledging the unreliability of our moral judgments and working to ameliorate it through careful understanding and designed countermeasures seems promising.

# Conclusion

Our moral judgments are probably unreliable. Even if this fact doesn't justify full [skepticism](https://plato.stanford.edu/entries/skepticism-moral/), it justifies serious attention. A fuller understanding of the limits of our moral faculties would help us determine how to respond.

# Appendix: Qualitative discussion of methodology

## Order

[@haidt1996social]
:   No immediate complaints here. I will note that order effects weren't the original purpose of the study and just happened to show up during data analysis.

[@petrinovich1996influence]
:   No immediate complaints.

[@lanteri2008experimental]
:   No immediate complaints.

[@lombrozo2009role]
:   No immediate complaints.

[@zamzow2009variations]
:   <blockquote>
    Interestingly, while we found judgments of the bystander case seem to be impacted by order of presentation, our results trend in the opposite direction of Petrinovich and O’Neill. They found that people were more likely to not pull the switch when the bystander case was presented last. This asymmetry might reflect the difference in questions asked---"what is the right thing to do?" versus "what would you do?"
    </blockquote>
    
    Or it might reflect noise.
    
[@wright2010intuitional]
:   No immediate complaints.

[@schwitzgebel2012expertise]
:   How many times can you pull the same trick on people? You kind of have to hope the answer is "A lot" for this study since it asked each subject 17 questions in sequence testing the order sensitivity of several different scenarios. The authors do acknowledge the possibility for learning effects.

[@liao2012putting]
:   No immediate complaints.

[@wiegmann2012order]
:   No immediate complaints.


## Wording

[@petrinovich1993empirical]
:    No immediate complaints.

## Disgust and cleanliness

[@wheatley2005hypnotic]
:    Hypnosis seems pretty weird. I'm not sure how much external validity hypnotically-induced disgust has. Especially after accounting for the fact that the results only include those who were successfully hypnotized---it seems possible that those especially susceptible to hypnosis are different from others in some way that is relevant to moral judgments.

[@schnall2008disgust]
:    In experiment 1, the mean moral judgment in the mild-stink condition was not significantly different from the mean moral judgment in the strong-stink condition despite a significant difference in mean disgust. This doesn't seem obviously congruent with the underlying theory and it seems slightly strange that this possible anomaly passed completely unmentioned.

     More concerning to me is that, in experiment 2, the disgust manipulation did not work as judged by self-reported disgust. However, the experimenters believe the "disgust manipulation had high face validity" and went on to find that the results supported their hypothesis when looking at the dichotomous variable of control condition versus disgust condition. When a manipulation fails to change a putative cause (as measured by an instrument), it seems quite strange for the downstream effect to change anyway. (Again, it strikes me as unfortunate that the authors don't devote any real attention to this.) It seems to significantly raise the likelihood that the results are reflecting noise rather than insight. 
     
     The non-significant results reported here were not, apparently, the authors' main interest. Their primary hypothesis (which the experiments supported) was that disgust would increase severity of moral judgment for subjects high in private body consciousness [@miller1981consciousness].

[@schnall2008clean]
:   The cleanliness manipulation in experiment 1 seems very weak. Subjects completed a scrambled-sentences task with 40 sets of four words. Control condition participants received neutral words while cleanliness condition participants had cleanliness and purity related words in half their sets. 

    Indeed, no group differences between the conditions were found in any mood category including disgust which seems plausibly antagonistic to the cleanliness primes. It's not clear to me why this part of the procedure was included if they expected both conditions to produce indistinguishable scores. It suggests to me that results for the manipulation weren't as hoped and the paper just doesn't draw attention to it? (In their defense, the paper is quite short.)

    The experimenters went on to find that cleanliness reduced the severity of moral judgment which, as discussed elsewhere, seems a bit worrying in light of the potentially failed manipulation.

    In experiment 2, "Because of the danger of making the cleansing manipulation salient, we did not obtain additional disgust ratings after the hand-washing procedure." which seems problematic given possible difficulties with manipulations by this lead author elsewhere in this paper and by this lead author in another paper from the same year.

    Altogether, this paper strikes me as very replication crisis-y. (I think especially because it echoes the infamous study about priming young people to walk more slowly with words about aging [@doyen2012behavioral].) (I looked it up after writing all this out and it turns out [others agree](https://replicationindex.com/category/simone-schnall/).)

[@horberg2009disgust]
:   No immediate complaints.

[@liljenquist2010smell]
:    I'm tempted to say that experiment 1 is one of those results we should reject just because [the effect size is implausibly big](http://daniellakens.blogspot.com/2017/07/impossibly-hungry-judges.html). "The only difference between the two rooms was a spray of citrus-scented Windex in the clean-scented room" and yet they get a Cohen's $d$ in a variant on the dictator game of 1.03. This would mean ~85% of people in the control condition would share less than the non-control average. If an effect of this size were real, it seems like we'd have noticed and be dousing ourselves with Windex before tough negotiations.

[@zhong2010clean]
:    This study found evidence for the claim that participants who cleansed their hands judged morally-inflected social issues more harshly. Wait, what? Isn't that the opposite of what the other studies found? Not to worry, there's a simple reconciliation. The cleanliness and disgust primes in those other studies were somehow about the target of judgment whereas the cleanliness primes in this study are about cleansing the *self*.

     It also finds that a dirtiness prime is no different than the control condition but, since it's primarily interested in the cleanliness prime, it makes no comment on this result.

[@eskine2011bad]
:   No immediate complaints.

[@david2011effect]
:   It's a bit weird that their evaluative conditioning procedure produced positive emotions for the control word which had been paired with neutral images. They do briefly address the concern that the evaluative conditioning manipulation is weak.

    Kudos to the authors for not trying too hard to explain away the null result: "This finding questions the generality of the role between disgust and morality.".
 
[@tobia2013cleanliness]
:   Without any backing theory predicting or explaining it, the finding that "the cleanliness manipulation caused students to give higher ratings in both the actor and observer conditions, and caused philosophers to give higher ratings in the actor condition, but lower ratings in the observer condition." strikes me as likely to be noise rather than insight.
 
[@huang2014does]
:    The hypothesis under test in this paper was that response effort moderates the effect of cleanliness primes. If we ignore that and just look at whether cleanliness primes had an effect, there was a null result in both studies.

     This kind of militant reluctance to falsify hypothesis is part of what makes me very skeptical of the disgust/cleanliness literature:
     
     <blockquote> 
     Despite being a [failed] direct replication of SBH, JCD differed from SBH on at least two subtle aspects that might have resulted in a slightly higher level of response effort. First, whereas undergraduate students from University of Plymouth in England “participated as part of a course requirement” in SBH (p. 1219), undergraduates from Michigan State University in the United States participated in exchange of "partial fulfillment of course requirements or extra credit" in JCD (p. 210). It is plausible that students who participated for extra credit in JCD may have been more motivated and attentive than those who were required to participate, leading to a higher level of response effort in JCD than in SBH. Second, JCD included quality assurance items near the end of their study to exclude participants "admitting to fabricating their answers" (p. 210); such features were not reported in SBH. It is possible that researchers’ reputation for screening for IER resulted in a more effortful sample in JCD.
     </blockquote>

[@johnson2014does]
:    Wow, much power--0.99. This is a failed replication of [@schnall2008clean].

[@johnson2016effects]
:    The power level for study 1, it's over 99.99%! This is a failed replication of [@schnall2008disgust].
  
[@ugazio2012role]
:   I excluded this study from quantitative review because they did this: "As the results obtained in Experiment 1a did not replicate previous findings suggesting a priming effect of disgust induction on moral judgments [...] we performed another experiment [...] the analyses that follow on the data obtained in Experiments 1a and 1b are collapsed". Pretty egregious.

## Gender

[@lombrozo2009role]
:   No immediate complaints.

[@seyedsayamdost2015gender]
:   This is a failed replication of [@buckwalter2014gender]. I didn't include [@buckwalter2014gender] in the quantitative review because it wasn't an independent experiment but selective reporting by design: "Fiery Cushman was one of the researchers who agreed to look for gender effects in data he had collected [...]. One study in which he found them [...]" [@buckwalter2014gender].

[@adleberg2015men]
:   No major complaints.

    They did do a post hoc power analysis which [isn't quite a real thing](http://daniellakens.blogspot.com/2014/12/observed-power-and-what-to-do-if-your.html).

## Culture and socioeconomic status

[@haidt1993affect]
:   They used the Bonferroni procedure to correct for multiple comparisons which is good. On the other hand:

    <blockquote>
    Subjects were asked to describe the actions as perfectly OK, a little wrong, or very wrong. Because we could not be certain that this scale was an interval scale in which the middle point was perceived to be equidistant from the endpoints, we dichotomized the responses, separating perfectly OK from the other two responses.
    </blockquote>

    Why did they create a non-dichotomous instrument only to dichotomize their own instrument after data collection? I'm worried that the dichotomization was done post hoc upon seeing the non-dichotomized data and analysis.

## Personality

[@feltz2008fragmented]
:   No immediate complaints.

## Actor/observer

[@nadelhoffer2008actor]
:   No immediate complaints.

[@tobia2013moral]
:   No immediate complaints.

<hr class="references">

<!-- By "[moral intuition]{.noted}[^intuition-def]", we will mean an immediately available sense of the moral status of some proposition---a sense that is available without extensive explicit or formal reasoning. If told "The deliberate humiliation, rape and torture of a child, for no purpose other than the pleasure of the one inflicting such treatment is immoral." [@shafer2008defending], you would (presumably) readily agree without any need to appeal to particular ethical theories or to explicitly weight costs and benefits. Philosophical intuitions are sometimes analogized to sensory perception---both perception and philosophical intuition can produce strong sensations of tracking an independent reality but their mechanisms of operation are fairly opaque. -->

<!-- [^intuition-def]: In the literature, there are a variety of subtle variations in the definitions and descriptions of philosophical intuition, but they all seem to be clustered around a concept like the one described above. Furthermore, I don't think of any of the subsequent work hinges on the precise definition so we'll not get too fussed about it. -->

<!-- ## Why do we care about moral intuitions? -->

<!-- The basic answer is that they seem to play a crucial role in both major accounts of moral reasoning. -->

<!-- ### Intuitionism -->

<!-- One possible mode of moral reasoning is simply to rely upon moral intuitions as the core and perhaps sole ingredient of moral judgments. If your moral intuition says that stealing a loaf of bread is wrong, that's pretty much the beginning and the end of it. You don't have to consider the consequences of the action or whether you will that it become a [universal maxim](https://en.wikipedia.org/wiki/Categorical_imperative). Moral intuitions are self-justifying. Philosophers who adhere to this view are labeled "intuitionists" [@tiberius2014moral]. -->

<!-- Since most people aren't familiar with explicit ethical systems (e.g. utilitarianism, Kantianism, virtue theory) as found in philosophy<sup>[citation needed]</sup>, they certainly cannot be said to knowingly deploy those systems when faced with moral dilemmas. Pure intuitionism then seems likely to be quite common in [folk morality]{.noted}[^parsimony]. But perhaps people are applying systematized folk theories of morality? We'll examine this next. -->

<!-- [^parsimony]: Parsimony does not mean that moral reasoning dominated by intuitions is simple or bad. We'll examine some of the many theoretical accounts of intuitions [later](#indirect-theoretical-evidence). -->

<!-- ### Constructivism -->

<!-- Another mode of moral reasoning is using moral intuitions as inputs to some "higher level" moral theory. From a folk morality perspective, this might look like mastering your visceral disgust reaction upon hearing that a brother and sister passionately kiss each other on the mouth and then reminding yourself that such behavior is consensual and (potentially) harmless. When constructivist [@tiberius2014moral] philosophers engage in this process, they talk about "[reflective equilibrium](https://plato.stanford.edu/entries/reflective-equilibrium/)". Reflective equilibrium is about: -->

<!-- 1. Feeding certain moral intuitions into a moral rule -->
<!-- 2. Finding the implications of that rule applied to those intuitions -->
<!-- 3. Determining whether those implications are acceptable i.e. whether bullets will be bit -->
<!-- 4. If the implications aren't acceptable, changing either the moral rule endorsed in step 1 or the set of moral intuitions endorsed in step 1 -->
<!-- 5. Repeating steps 1 through 4 until the implications of endorsed intuitions and rules are acceptable i.e. reach equilibrium -->

<!-- More succinctly, [reflective equilibrium]{.noted}[^refl-def] is about bouncing back and forth between intuitions and rules until your intuitions dictate rules you can accept and your rules dictate intuitions you can accept. At any rate, the point in all this is that even in these more involved processes of moral reasoning, moral intuitions play a key role. There's no practical ethical theory that is purely formal and entirely ungrounded in moral intuitions. -->

<!-- (It may be somewhat useful to analogize the two modes of reasoning endorsed by intuitionists and constructivists to [system 1 and system 2](TODO). However, I wouldn't put too much weight on that because the similarity may be more coincidental than fundamental and there seem to be some important disanalogies.) -->

<!-- [^refl-def]: This reflective equilibrium perspective also suggests a slightly different definition of moral intuition from the ones discussed above. We could just call moral intuitions "the data fed into higher level moral theories" which is perhaps closest to what we truly care about when considering reliability; we ultimately care about the reliability of the system of moral reasoning as a whole more than the reliability of any arbitrary subcomponent. -->

<!-- <blockquote> -->
<!-- moral intuitions are unreliable to the extent that morally irrelevant factors affect moral intuitions. [[@doris2010moral]]{.attribution} -->
<!-- </blockquote> -->

<!-- (Moral intuitions are not the same as moral judgments, but they are plausibly an important constituent of moral judgments. We'll look at this in more detail later. [TODO]) -->

<!-- In the first mode of moral reasoning---in which moral intuitions play the dominant role in moral judgments, it's [obvious]{.noted}[^metaethics] why pervasive and severe unreliability poses a problem. Unreliable intuitions means unreliable judgment and individuals will act wrongly while perceiving themselves to act rightly. -->

<!-- In the second mode of moral reasoning---in which moral intuitions are paired with higher level moral rules, pervasive and severe unreliability likely also poses a problem. The slogan here is "Garbage in, garbage out". (While I think <abbr title="Garbage in, garbage out">GIGO</abbr> is a real concern, I don't think it's a tautology. We'll examine it more closely later.) -->
