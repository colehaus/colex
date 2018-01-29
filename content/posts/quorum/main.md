---
title: A quorum alternative
published: 2015-01-28
tags: voting, bayes, frequentist, statistics
css: quorum
js: /js/jquery.flot.min.js, /js/jstat.min.js, /js/plot.js, /js/mcmc.js, /js/quorum.js, /js/quorum-map.js
---

<div id="graph-of-contents"><a href="#arg-map">Contents</a></div>

<div class="abstract">
Voting procedures can replace traditional quorum with the use of statistical
techniques. If aggregate opinion on the alternatives is "similar", according to
those techniques, we declare a failure of quorum.
</div>

# Motivating examples

<ol><li id="example1">Pie Club is voting on which pie will be featured at
their first August meeting. <span class="noted">After tallying the votes,
[buko pie](https://en.wikipedia.org/wiki/Buko_pie) receives a mean score of
$0.69$ and [fish pie](https://en.wikipedia.org/wiki/Fish_pie) receives a mean
score of $0.18$.</span>[^range]

Before the decision is finalized, however, an observant member notices that the
meeting is <a href="#arg-map" id="restrictive">two members short of the 25
required for quorum</a>. Because Pie Club is scrupulously democratic, the vote
is annulled. Some members grumble their doubt that the landslide will reverse
with two more votes.</li>

<li id="example2">Pie Club is voting on which pie will take home the title "Pie
of the Decade". Will it be
[lemon meringue pie](https://en.wikipedia.org/wiki/Lemon_meringue_pie) or
[Tarta de Santiago](https://en.wikipedia.org/wiki/Tarta_de_Santiago)? The
results are in---quorum checked in advance this time---and they are... $0.49$
for meringue and $0.48$ for Tarta. While meringue's devotees celebrate, Tarta's
die-hards feel something has gone wrong. <a href="#arg-map" id="permissive">Can
such a close result really give them confidence</a> that meringue is the
preference of the whole club, including the 12 members who couldn't make it to
meeting? If just one of them had attended and cast a vote favoring Tarta,
wouldn't that have swung the outcome?</li></ol>

<!--more-->

# Quorum

<blockquote>
<a href="#arg-map" id="quorum-def">The minimum number of members who must be
present at the meetings</a> of a deliberative assembly for business to be
validly transacted is the *quorum* of the assembly. The requirement of a quorum
is a protection against totally unrepresentative action in the name of the body
by an unduly small number of persons. [@ronr]
</blockquote>

So quorum is a proxy for representativeness. But as the examples demonstrate,
it's, at best, a loose proxy. Sometimes (as in the [first example](#example1))
quorum is too demanding---it forbids a decision when the votes endorse one. On
other occasions (as in the [second example](#example2)), quorum is too lax---it
declares representativeness when there can be no certainty of it.

Is there an alternative then? How do we determine if a vote is representative?
Statistics! 

# Statistics

<ul class="switch" type="menu" menu="stat-type">
<li class="open">
## Bayesian
</li><li>
## Frequentist
</li></ul>

For each pair of alternatives, we'd like to find which of these three is true:

- $\mu_2 \gg \mu_1$
- $\mu_1 \gg \mu_2$
- $\mu_2 \approx \mu_1$

where $x \gg y$ means something like "We are justified in believing that
$x > y$." and $x \approx y$ means something like "We aren't justified in
believing that $x > y$ or that $y > x$.". The first two correspond to quorum and
the third corresponds to a failure of quorum.

To establish $\mu_2 \gg \mu_1$, we construct a lower
<span class="inline switch" type="menu" menu="stat-type">
<span class="open">credible bound (a one-sided
[credible interval](https://en.wikipedia.org/wiki/Credible_interval))</span>
<span>confidence bound (a one-sided
[confidence interval](https://en.wikipedia.org/wiki/Confidence_interval))</span>
</span>
on $\mu_2 - \mu_1$ base on our votes. The lower bound delimits the region where
$\mu_2 - \mu_1$ is largest. If the delimited region includes $0$, we must reject
$\mu_2 \gg \mu_1$ (i.e. if the region most favorable to $\mu_2$ still
doesn't exclude $0$, we aren't justified in believing $\mu_2 > \mu_1$ (at the
chosen
<span class="inline switch" type="menu" menu="stat-type">
<span class="open">credibility</span>
<span>confidence</span>
</span>
level)).

The approach for $\mu_1 \gg \mu_2$ is similar (simply swap in an upper bound or
$\mu_1 - \mu_2$).

If we reject $\mu_2 \gg \mu_1$ and $\mu_1 \gg \mu_2$, we must accept
$\mu_2 \approx \mu_1$ and declare a failure of quorum.

For example, we'd like to <a href="#arg-map" id="bayes">determine if the
<span class="inline switch" type="menu" menu="stat-type">
<span class="open">credible</span>
<span>confidence</span> 
</span>
bounds support the conclusion</a> that buko pie really is preferred to fish pie.
If Pie Club bylaws specified a $95\%$
<span class="inline switch" type="menu" menu="stat-type">
<span class="open">credible</span>
<span>confidence</span> 
</span>
bound and the lower bound for $\mu_{buko} - \mu_{fish}$ stretched to $0.359$
while the upper bound stretched to $0.58$, we'd declare that quorum had been
reached in favor of buko pie. (We reject $\mu_{fish} \gg \mu_{buko}$ because
it's bounded region includes $0$. We can't reject $\mu_{buko} \gg \mu_{fish}$
because it's bounded region excludes $0$.) Alternately, if the lower bound
stretched to $-0.1$ and the upper bound stretched to $0.58$, we'd declare a
failure of quorum.

<ul class="switch" type="menu" menu="stat-type">
<li class="open">
How do we construct these credible bounds? We derive them from the
[posterior probability distribution](https://en.wikipedia.org/wiki/Posterior_probability)
created using Bayesian parameter estimation [@kruschke13]. To construct this
posterior, we start by specifying <span class="noted">a model for the
distribution of paired differences</span>[^difference]. Because the
difference can only take on values in the interval $\left(-1, 1\right)$, the
([transformed](https://en.wikipedia.org/wiki/Location-scale_family) from
$(0, 1)$ to $(-1, 1)$)
<span class="noted">[beta distribution](https://en.wikipedia.org/wiki/Beta_distribution)
is a sensible choice</span>[^beta]. To get a sense of the beta distribution, you
can look at the calculator
[here](http://keisan.casio.com/has10/SpecExec.cgi?id=system/2006/1180573226).

Now that we have a model of differences, we must choose
[prior probability distributions](https://en.wikipedia.org/wiki/Prior_probability)
for its parameters. Note that $\alpha = \beta = 1$ collapses the beta
distribution to the uniform distribution. Because the uniform distribution is
the
[maximum entropy distribution](https://en.wikipedia.org/wiki/Maximum_entropy_probability_distribution)
on a supported interval, we should choose prior distributions of $\alpha$ and
$\beta$ with means of $1$ [@sivia06]. The maximum entropy distribution with mean
$1$ supported on $\left(0, \infty\right)$ is the exponential distribution with
$\lambda = 1$. So the prior on each of $\alpha$ and $\beta$ is $Exp(1)$. All of
this is diagrammatically represented in the accompanying figure.

<figure id="distrogram">
  <img src="/images/quorum/distro.png" alt="Diagrammatic representation of
  distributions and dependencies">
  <figcaption>
  We model the vote differences with a beta distribution. That beta
  distribution's parameters have priors of $Exp(1)$.
  </figcaption>
</figure>

Now that we have our data and prior probability distributions,
[Bayes' theorem](https://en.wikipedia.org/wiki/Bayes'_theorem) allows us to
reallocate probability mass to form the posterior probability distributions. We
generate numerical estimates of the posterior probability distributions using
adaptive Metropolis-within-Gibbs [@roberts09] [@sumsar-best]. These posteriors
on the parameters of beta allow us to straightforwardly calculate the posterior
on the mean of the difference using the formula for the mean of a beta
distribution.
</li>
<li>
How do we construct these confidence bounds? That depends.

<ul class="switch" type="menu" menu="sample-size">
<li class="open">
If the number of votes is "large" (say, more than 30) (and the distribution of
votes satisfies some other
[conditions](https://en.wikipedia.org/wiki/Central_limit_theorem)), we can do
<span class="noted">a $t$-test on the paired differences of the
votes.</span>[^difference]
</li>
<li>
If the number of votes is "small" (say, 30 or fewer), we have a problem. We
cannot assume that the underlying distribution of votes is
[normal](https://en.wikipedia.org/wiki/Normal_distribution). Voters could be
polarized <span class="spark" id="non-norm"></span>, for example. And because
the number of votes is small, we can't rely on the
[central limit theorem](https://en.wikipedia.org/wiki/Central_limit_theorem) to
approximate normality. So we must use a
[distribution-free](https://en.wikipedia.org/wiki/Nonparametric_statistics)
(sometimes called non-parametric) method.

The distribution-free test for paired data is the
[Wilcoxon signed-rank test](https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test)
[@wilcoxon45]. However, this test requires the symmetrical distribution of
paired differences. Again, we cannot <span class="spark" id="sym1"></span>
assume <span class="spark" id="sym2"></span> that.

So, as far as I know, there's no simple frequentist procedure which is
applicable in the small sample case.
</li>
</ul>
</li>
</ul>

<span class="noted">You can try it out below</span>[^single-bound].
Maybe look for:

- A scenario in which the quorum status (achieved or failed) depends on the
  credibility level chosen (e.g. $95\%$ or $99\%$)
- Two scenarios in which each alternative retains its mean score, but the quorum
  status changes
- Two scenarios in which the number of voters remains the same, but the quorum
  status changes

<form>
  <div class="data-input">
  <div>
  <label>Alternative 1</label>
  <textarea id="data1">0.10 0.00 0.01 0.45 0.40 0.00 0.01 0.12 0.02 0.00 0.15 0.29 0.19 0.08 0.12 0.09 0.17 0.95 0.01 0.00 0.64 0.17 0.07 0.02 0.21 0.31 0.18 0.45 0.18 0.12</textarea>
  <output><figure id="preview1"><div></div></figure></output>
  </div>
  <div>
  <label>Alternative 2</label>
  <textarea id="data2">0.61 0.58 0.53 0.70 0.71 0.64 0.57 0.61 0.43 0.70 0.85 0.61 0.66 0.70 0.74 0.80 0.59 0.37 0.81 0.72 0.52 0.69 0.73 0.84 0.91 0.93 0.84 0.85 0.73 0.61</textarea>
  <output><figure id="preview2"><div></div></figure></output>
  </div>
  </div>
  <output><figure id="diff"><figcaption>Differences (Prop 2 - Prop 1)</figcaption><div></div></figure></output>
  <div class="act">
  <button class="preview" type="button">Update Preview</button>
  <button class="analyze" type="button">Analyze</button>
  <progress value="0"></progress>
  </div>
  <output id="stat-out">
  <ul class="switch" type="menu" menu="stat-type">
  <li class="open">
  <div id="best">
  <figure id="mean"><figcaption>Mean of differences</figcaption><div></div></figure>
  <div id="kuwa-parms">
  <figure id="alpha"><figcaption>Alpha</figcaption><div></div></figure>
  <figure id="beta"><figcaption>Beta</figcaption><div></div></figure>
  </div>
  </div>
  </li>
  <li>
  <figure id="freq"><figcaption>Mean of differences</figcaption><div></div></figure>
  </li>
  </ul>
  </output>
</form>
</li>

# Problems

We've tacitly <a href="#arg-map" id="random">assumed that our actual voters are
a random sample</a> of the population of potential voters. This is false (Though
one could make it true through adoption of appropriate
[voting procedures](https://en.wikipedia.org/wiki/Sortition)).
[Self-selection bias](https://en.wikipedia.org/wiki/Self-selection_bias),
whether due to differential interest, availability, transportation, &c., means
that the sample is non-representative. However, the problem of non-random
samples also applies to traditional quorum's assurances of representativeness.

The procedure also accepts the claim of [@ronr] that the purpose of quorum is to
ensure representativeness. In consequence, the procedure takes votes as
exogenous and characterizes only the resulting information. But one could
support quorum for its deliberative, community-building, or even obstructive
effects.

Also, <a href="#arg-map" id="complicated">isn't all this math a bit
forbidding?</a>

<ul class="switch" type="menu" menu="stat-type"><li class="open">
The conclusions have a rather intuitive interpretation in terms of likelihood,
maybe even more intuitive than the traditional quorum interpretation ("We ensure
that our decisions are representative by requiring 25% of our members to
attend."). But getting to the conclusions requires a computer and uncommon
math.
</li><li>
The math itself may be familiar to some, but there are still the
perennial problems of frequentist interpretation [@goodman08]:

<div class="conversation">
"Neat. The test showed it's highly likely that buko pie is the preferred pie?"

"No, it either is or isn't the preferred pie. There's no probability involved."

"What was all that $5\%$ talk then?"

"Of all confidence intervals constructed at the $95\%$ level, $95\%$ should
contain the true population difference."

"Uhh..."
</div>
</li></ul>

So this procedure is less accessible than traditional quorum. How much less
depends on the relative importances placed on accessible conclusions and
accessible process.

Because this calculation is less accessible and will likely be performed by one
or a few individuals, it's import to establish a clear and strict procedure.
We should not permit, for example, the choice of modeling distribution after
exploratory data analysis. The more degrees of freedom we give the analyst, the
more power we give them to influence results
[@simmons11].

Finally, this procedure <a href="#arg-map" id="post-hoc">admits only post-hoc
declarations of quorum</a>. With the traditional procedure, we can just take
attendance at a meeting and determine the quorum status for every referendum
therein. With the new procedure, after tallying the votes on an issue, we have
to run the quorum calculation to retroactively determine quorum if we achieved
quorum.

# Extended procedures

<ul class="switch" type="menu" menu="stat-type">
<li class="open">
## Non-parametric

As mentioned, the procedure specified above assumes that the paired differences
are amenable to modelling by a beta distribution. This leads to overconfident
inferences [@hoeting99]. The assumption can be relaxed through the use of
Bayesian
[non-parametric](https://en.wikipedia.org/wiki/Nonparametric_statistics) methods
[@walker99]. The idea is to use a model with an infinite number of parameters
and marginalize out surplus dimensions on our finite data.

## Tying

We can also modify the procedure to permit tying. Under standing voting
procedures, scores of $0.490$ to $0.488$ produce an identical outcome (i.e.
victory for the first option) to scores of $0.82$ to $0.12$. However, if those
values are accurate estimates of the population scores (i.e. the uncertainty is
small), one could argue that the former scenario suggests a compromise or
synthesis position.

By defining "regions of practical equivalence", parameter estimation allows the
possibility of a tie [@kruschke13]. For example, Pie Club could decide that a
difference of mean scores smaller than 0.01 counts as a tie. If the credible
interval on the difference of means is contained entirely in this region, we
don't have a failure of quorum (uncertainy about which option is preferred), but
certainty that neither option is substantially preferred.

## Multiple alternatives

For simplicity, we looked at referenda with only two alternatives. We can extend
the procedure to referenda with more alternatives. The most straightforward
method would be to apply the procedure described above pairwise. The credibility
level would need to adjusted by something like the
[Šidák correction](https://en.wikipedia.org/wiki/%C5%A0id%C3%A1k_correction) to
deal with the
[problem of multiple comparisons](https://en.wikipedia.org/wiki/Multiple_comparisons_problem). A
better solution would be to perform a Bayesian
[ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance) analogue with
follow-up tests [@wetzels12].
</li>
<li>
## Multiple alternatives

For simplicity, we looked at referenda with only two alternatives. We can extend
the procedure to referenda with more alternatives. The most straightforward
method would be to apply the procedure described above pairwise. The confidence
level would need to adjusted by something like the
[Šidák correction](https://en.wikipedia.org/wiki/%C5%A0id%C3%A1k_correction) to
deal with the
[problem of multiple comparisons](https://en.wikipedia.org/wiki/Multiple_comparisons_problem). A
better solution would be to perform
[ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance) with follow-up
tests.
</li>
</ul>

## Non-ordinal outcomes

The procedure described above applies to ordinal outcomes. That is, we were
looking for the alternative with a score higher than all others. If we are
trying to assess outcomes on
[a ratio or interval scale](https://en.wikipedia.org/wiki/Level_of_measurement),
we'd have to use a different procedure.

The appropriate modification depends on the issue at hand. In cases where
caution is required, an organization could dispense with quorum and simply use
the conservative bound. In other circumstances, an organization could place
limits on the maximum size of the interval.

For example, suppose Pie Club put the size of its budget for the next year to a
vote. If it were feeling fiscally responsible, it could simply sets its budget
to the $95\%$ lower bound on the mean vote. An alternative would be to declare a
failure of quorum if the vote didn't produce a $95\%$ interval smaller than $\mu
\pm 10\%$.

# Future work

- Social welfare analysis versus traditional quorum [@bordley83]
- Human trials

[^range]: Throughout this post, I'll be working with continuous
[range voting](https://en.wikipedia.org/wiki/Range_voting):

    Polling rule
    :   That is, each voter scores each option on a range from 0 to 1 (exclusive).
    Aggregation rule
    :   The individual scores are then averaged to arrive at an overall score.
    Decision rule
    :   The option with the highest mean is the winner.
The techniques described here could be extended to many alternate procedures.
[^difference]: When using paired samples (here, we'd like to pair each person's
vote on buko pie to their vote on fish pie), we can transform two samples into a
single sample of their differences. We can then use our single sample techniques
on $d_i = buko_i - fish_i$ where $y_i$ is the $i$th person's vote on proposal
$y$.
[^beta]: This entails the false assumption that our paired differences follow a
beta distribution. Later, we'll discuss possibilities for remedying this. For
the moment, the beta distribution gives passable results, for its simplicity.
[^single-bound]: For visual clarity, the plots below show only the bound which
"crosses $0$ the least". If and only if this bounded region includes $0$, quorum
has failed.

<menu id="sample-size" type="popup">
  <menuitem label="Many votes" type="radio" checked="checked"></menuitem>
  <menuitem label="Few votes" type="radio"></menuitem>
</menu>

<menu id="stat-type" type="popup">
  <menuitem label="Bayesian" type="radio" checked="checked"></menuitem>
  <menuitem label="Frequentist" type="radio"></menuitem>
</menu>

<hr class="references">
