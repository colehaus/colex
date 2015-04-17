---
title: Toward an alternative bibliometric
published: 2014-11-08
tags: bibliometrics, science, publishing, bayes, information-theory
js: /js/bibliometric.min.js
css: bibliometric
---

<div class="abstract">
Impact factor isn't great. A bibliometric based on entropy reduction may be
promising. 
</div>

# Impact factor

There are a variety of citation-based
[bibliometrics](https://en.wikipedia.org/wiki/Bibliometrics). The current
dominant metric is
[impact factor](https://en.wikipedia.org/wiki/Impact_factor). It is highly
influential, factoring into decisions on promotion, hiring, tenure, grants and
departmental funding [@plos06] [@agrawal05] [@moustafa14]. Editors
preferentially publish review articles and push authors to
[self-cite](https://en.wikipedia.org/wiki/Coercive_citation) in pursuit of
increased impact factor [@plos06] [@agrawal05] [@wilhite12]. It may be
responsible for editorial bias against replications [@neuliep90]
[@brembs13]. Consequently, academics take impact factor into account throughout
the planning, execution and reporting of a study [@plos06].

This is [Campbell's law](https://en.wikipedia.org/wiki/Campbell's_law) in
action. Because average citation count isn't what we actually value, when it
becomes the metric by which decisions are made, it distorts academic research.
In the rest of this post I propose a bibliometric that measures the
[entropy reduction](https://en.wikipedia.org/wiki/Entropy_(information_theory))
of the [research graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph),
on the supposition that this hews more closely to what we value.

<!--more-->

$$
\newcommand{cond}[3] {
  #1\mathopen{}\left(#2\mathbin{\big|}#3\right)\mathclose{}
}
$$

# Entropy

Claude Shannon codified entropy as
$H(X) = -\sum\limits_{i} P(x_i) \log_2 P(x_i)$
where $x_i$ are the possible values of a discrete random variable $X$
[@shannon48][@cover12]. For example, the entropy of a 6-sided die is
$$\begin{align}
  H(D) &= - P(⚀) \log_2 P(⚀) - P(⚁) \log_2 P(⚁) - P(⚂) \log_2 P(⚂) \\
       & - P(⚃) \log_2 P(⚃) - P(⚄) \log_2 P(⚄) - P(⚅) \log_2 P(⚅) \\
       &= -\left(6 \left(\frac{1}{6} \log_2 \frac{1}{6}\right)\right) \\
       &= \log_2 6
\end{align}$$.

If we next learn that the die is weighted and can only roll even numbers, this
changes the entropy (our uncertainty).

$$\begin{align}
H(D|\epsilon) &= - P(⚁) \log_2 P(⚁) -
                   P(⚃) \log_2 P(⚃) -
                   P(⚅) \log_2 P(⚅) \\
     &= - \left(3 \left(\frac{1}{3} \log_2 \frac{1}{3}\right)\right) \\
     &= \log_2 3
\end{align}$$

<span class="noted">So the reduction in uncertainty is
$H(D) - H(D|\epsilon) = \log_2 6 - \log_2 3 = 1$.</span>[^intuition]

# Example

We can use these definitions to calculate the information provided by a research
paper and assign an Infometric®™ score. We'll start with a fairly classic
example about cigarette smoking.

## First study

Suppose we do a study on whether, in the normal course of smoking, cigarette
smoke is inhaled into the lungs. Prior to the study we use the (extremely)
uninformative prior $\cond{P}{A=t}{} = 0.5$. After the study (which we'll call
$\alpha$) we perform a
[Bayesian update](https://en.wikipedia.org/wiki/Bayes'_theorem) and find that
$\cond{P}{A=t}{\alpha} = 0.8$. So our study has provided

$$\begin{align}
H(A) - \cond{H}{A}{\alpha} &= -P(A=t)\log_2P(A=t) - P(A=f)\log_2P(A=f) \\
  &+ \cond{P}{A=t}{\alpha}\log_2\cond{P}{A=t}{\alpha} \\
  &+ \cond{P}{A=f}{\alpha}\log_2\cond{P}{A=f}{\alpha} \\
  &= 2 \left(-0.5\log_20.5\right) \\
  &+ 0.8\log_20.8 \\
  &+ 0.2\log_20.2 \\
  &\approx 0.278
\end{align}$$

bits of entropy reduction. Thus its score at the moment is
$0.278$. So far, so good?

## Second study

Now, we wish to study whether smoking causes chronic bronchitis. Suppose the
study we design pipes smoke directly into the lungs of experimental
subjects. The validity of our conclusion (Smoking does (not) cause chronic
bronchitis.) now depends on the truth of the claim that cigarette smoke is
inhaled into the lungs. So this new study is dependent on the prior study and
will cite it.

<figure>
  <img src="/images/bibliometric/bronchitis-pre.svg"
  alt="Graph depicting conditional dependencies" width="300" height="300">
  <figcaption>
  We are using uninformative priors.

  (In this and subsequent graphs, we follow the
  conventions of
  <a href="https://en.wikipedia.org/wiki/Bayesian_network">Bayesian networks</a>
  (i.e. a cited paper is the parent rather than the child---the arrow runs from
  rather than to the cited paper) rather than the conventions of
  <a href="https://en.wikipedia.org/wiki/Citation_graph">citation graphs</a>.)
  </figcaption>
</figure>

Now we carry out our study (we'll call it $\beta$). It provides evidence that
cigarette smoking does lead to bronchitis (conditional on the supposition that
cigarette smoke is inhaled into the lungs). So we update our
$\cond{P}{B=t}{\beta}$. The entropy reduction from this study, considered in
isolation, is $H(A,B) - \cond{H}{A,B}{\beta} \approx 0.266$.

<figure>
  <img src="/images/bibliometric/bronchitis-post.svg"
  alt="Graph depicting conditional dependencies" width="300" height="300">
  <figcaption>
  We have integrated data from the second study.
  </figcaption>
</figure>

But what if we don't consider it in isolation? First, we look for the total
entropy reduction from both studies and find 
$H(A,B) - \cond{H}{A,B}{\alpha,\beta} \approx 0.703$. <span class="noted">Note
that this is not simply the sum of the isolated reductions.</span>[^sum] 

<figure>
  <img src="/images/bibliometric/bronchitis-both.svg"
  alt="Graph depicting conditional dependencies" width="300" height="300">
  <figcaption>
  We have integrated data from both studies now.
  </figcaption>
</figure>

How do we apportion this gain into Infometric®™ scores then? We can decompose
the aggregate gain into a sum like

$$\begin{align}
H(A,B) - \cond{H}{A,B}{\alpha,\beta} &=
    \cond{H}{A,B}{\beta} - \cond{H}{A,B}{\alpha,\beta} \\
  &+ H(A,B) - \cond{H}{A,B}{\beta}
\end{align}$$

where $\cond{H}{A,B}{\beta} - \cond{H}{A,B}{\alpha,\beta} \approx 0.437$
represents $\alpha$'s score and $H(A,B) - \cond{H}{A,B}{\beta} \approx 0.266$
represents $\beta$'s score.

(the general form is
$H(S_1,S_2,\cdots,S_n) -
\cond{H}{S_1,S_2,\cdots,S_n}{\sigma_1,\sigma_2,\cdots,\sigma_n} =
\sum\limits_{i=1}^n I(\sigma_i))$ where
$I(\sigma_i) = \cond{H}{S_1,S_2,\cdots,S_i}{\sigma_{i+1},\sigma_{i+2},\cdots,\sigma_n}
- \cond{H}{S_1,S_2,\cdots,S_i}{\sigma_i,\sigma_{i+1},\sigma_{i+2},\cdots,\sigma_n}$

We can see that $\beta$ citing $\alpha$ has increased $\alpha$'s score ($\alpha$
now reduces our uncertainty not only about $A$, but also about $B$), a "citation
bonus".

## Fourth study

We'll now jump to a fourth study so we can examine a fuller set of interactions
(i.e. multiples studies citing one study, one study citing multiple studies). 

<figure>
  <img src="/images/bibliometric/four.svg"
  alt="Graph depicting conditional dependencies" width="500" height="500">
</figure>

The decomposition

$$\begin{align}
H(A,B,C,D) - \cond{H}{A,B,C,D}{\alpha,\beta,\kappa,\delta} &=
    \cond{H}{A,B,C,D}{\beta,\kappa,\delta} -
    \cond{H}{A,B,C,D}{\alpha,\beta,\kappa,\delta} \\
  &+ \cond{H}{A,B,C,D}{\kappa,\delta} -
    \cond{H}{A,B,C,D}{\beta,\kappa,\delta} \\
  &+ \cond{H}{A,B,C,D}{\delta} - \cond{H}{A,B,C,D}{\kappa,\delta} \\
  &+ H(A,B,C,D) - \cond{H}{A,B,C,D}{\delta} \\
\end{align}$$ leads to scores of $I(\alpha) = 0.547$, $I(\beta) = 0.387$,
$I(\kappa) = 0.123$, and $I(\delta) = 0.434$.

# Demo

Try it out:

<form class="net">
<div>
<textarea>
<verbatim>

    ----
    | A P
    ----
    | t 0.8
    | f 0.2

    ----
    A | B P
    ----
    t | t 0.9
      | f 0.1
    f | t 0.5
      | f 0.5

    ----
    A | C P
    ----
    t | t 0.7
      | f 0.3
    f | t 0.6
      | f 0.4

    ----
    B C | D P
    ----
    t t | t 0.99
        | f 0.01
    t f | t 0.9
        | f 0.1
    f t | t 0.8
        | f 0.2
    f f | t 0.55
        | f 0.45
</verbatim>
</textarea>
</div>
<button type="button">Calculate scores</button>
<div>
<output>
</output>
</div>
</form>

# Discussion

## Desirable properties

Score depends on study design
  :  e.g. *ceteris paribus*, a study with higher power will have a higher score
Aggregates sensibly
  :  The impact factor is often used in inappropriate circumstances [@plos06]
     [@agrawal05] [@moustafa14]. That is, the warning that impact factor is a
     metric for journals, not authors or departments, is seldom heeded. The
     proposed metric can used in such cases trivially. For example, if an author
     has published studies $\eta$ and $\theta$, their score is simply
     $I(\eta) + I(\theta)$, the total reduction in entropy they contribute.
Handles replications appropriately
  :  Impact factor tends to undervalue replications [@neuliep90] [@brembs13].
     With a simple extension of the proposed metric, if $\iota$ is a replication
     of $\eta$ about proposition $E$ it shares the "citation bonus" in
     proportion to how much it increases our certainty in $E$.
Gradated citations
  :  With impact factor, a citation to study $\alpha$ essential to the validity
     of study $\gamma$ is given the same weight as a citation to study $\beta$
     providing some minor context for $\gamma$. With the proposed metric, if
     $\gamma$ only depends minorly on $\beta$, $\gamma$ will only boost
     $\beta$'s score minorly. This should counteract the inflated value of
     review articles.

## Undesirable properties

Not [incentive compatible](https://en.wikipedia.org/wiki/Incentive_compatibility)
  :  For example, if study $\beta$ depends on study $\alpha$ it will receive a
     better score by hiding this dependence and marginalizing. For example,
     $\beta$ receives a higher score when presented as

         ----
         | A P
         ----
         | t 0.8
         | f 0.2

         ----
         | B P
         ----
         | t 0.82
         | f 0.18

     than when presented as

         ----
         | A P
         ----
         | t 0.8
         | f 0.2

         ----
         A | B P
         ----
         t | t 0.9
           | f 0.1
         f | t 0.5
           | f 0.5
     . However, when considering impact factor as a relative metric used for
     comparisons, it also theoretically discourages citation (e.g. citing the
     paper of a competing applicant for a position boosts their impact
     factor-derived score). This problem does not seem to be devastating
     [@liu93].
Complicated
  :  The proposed metric is more procedurally complicated than impact factor.
     (Though the actual impact factor calculation procedure is more complicated
     than one would naively suppose.)
Requires assessment of degree of dependence
  :  The "degree of dependence" (e.g. $\cond{P}{B}{A=t}$ vs. $\cond{P}{B}{A=f}$)
     occupies an important role in the procedure. It's not obvious to me how
     this should be determined other than by discussion between authors, editors
     and reviewers.

# Future work

- Improve interface of demonstration
- Extend to replications and multi-proposition studies
- Compare with impact factor on real corpus
- Compare with expert evalution on real corpus

[^intuition]: The intuition behind this result is something like our uncertainty
is halved (1 bit) because one half of the fair die states are no longer
possible.
[^sum]: This accords with the intuition that value of two facts considered
together is not simply the sum of their separate values (e.g. learning that Fido
is small is largely redundant once you've learned that Fido is a Chihuahua).

<hr class="references">

