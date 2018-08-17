---
title: Ideal theory and decision theory
date: 2018-08-15
tags: ideal theory, decision theory, political philosophy
series: The Tyranny of the Ideal
---

# Ideal theory

If I may editorialize, the ideal theory debate is essentially about how to translate our understanding of justice into actions in the present. Reductively, one side (the idealists) advocates for always moving the world we inhabit closer to the ideally just world while the other side (the non-idealists) advocates for always moving the world we inhabit toward the best adjacent world.

What's not usually at issue in the ideal theory debate is: our understanding of the status quo, our predictive models of the future, or our notion of justice. That's not to say that there's consensus on these issues---far from it. It's just that discussion of these issues doesn't fall under the heading of 'ideal theory'. No one considers themselves to be waging that debate when they talk about currently existing inequality in Germany or what justice recommends with regard to positive and negative rights. By all this I merely mean to emphasize that the scope of the ideal theory debate is rather small---given all the presuppositions above, what algorithm do we employ to choose the next possible world we'll inhabit?

Hopefully, by framing the ideal theory debate in the foregoing terms, I've predisposed you to my point of view: The subject matter of the ideal theory debate is also the subject matter of decision theory. That is, the ideal theory debate is really a debate about applied decision theory.

# Normative decision theory

~~[Webster's dictionary defines](http://www.patheos.com/blogs/religionprof/2016/12/websters-dictionary-defines.html)~~---\*cough\*---[@hansson1994] says "decision theory is concerned with goal-directed behaviour in the presence of options". We'll try to make this description more comprehensive by appealing to [Leonard Savage's formalization](https://plato.stanford.edu/entries/decision-theory/#SavThe). The hope is that by describing decision theory fully, we can see how the boundaries of the ideal theory debate line up with the boundaries of decision theory.

<!--more-->

Savage starts by highlighting a set $S$ of initial states of the world, a set $O$ of outcomes, and a set $F$ of actions represented by functions from $S$ to $O$. $f(s_i)$ denotes the outcome of action $f \in F$ when state $s_i \in S$ is the actual state of the world. Furthermore, we have a probability function $P$ from states $S$ to probabilities $\mathbb{I}$ and a utility function $u$ from outcomes $O$ to utilities $\mathbb{R}$.

Each of these elements has a parallel in the overall problem of social engineering. Descriptive social sciences like anthropology and sociology all advance different (overlapping) descriptions of the existing world---these correspond to Savage's set $S$ of world states. Our uncertainty about which description is correct corresponds to the probability function over states $P$. Things like emancipatory social science [@wright2010eru], in part, outline the set of actions $F$ available to transform our world into some outcome $o \in O$. The task of determining which outcomes correspond to which actions is the job of [predictive social science like economics]{.noted}[^predictive-social]. The proper utility function $u$ assigning values to outcomes is debated by ethicists and political philosophers.

Obviously, there are a lot of moving pieces here because the problem of social engineering is complex. After apportioning out all the above responsibilities to different fields of study, what's left over? In the purely formal context, we still have to determine which action in $F$ to take, in the face of all our assumptions about $S$, $O$, $F$, $P$, and $u$, and what rules generate that recommendation---this is decision theory. And in the social engineering context, we still have to determine which world we ought to try to move to, in the face of all our positive and normative beliefs, and what rules generate that recommendation---this is the ideal theory debate. As [@gauss2016] says, "[T]he question that confronts the political theorist: ... [W]hat moves (reforms) does our political philosophy recommend?".

# So what?

Let's pretend you're thoroughly convinced by this argument and move on. Is knowing that the ideal theory debate is actually about decision theory of any use? I think so. Here are some tools and concepts from decision theory that seem useful in the domain of ideal theory:

## von Neumann-Morgenstern utility theorem

[@gaus2016] says one of the key distinctions between ideal theory and non-ideal theory is their 'dimensionality'. Anti-idealists like Sen evaluate each world only with respect to its justice. Idealists are committed, he claims, to evaluating worlds on both their inherent justice and their proximity to the ideally just world. Anti-idealists are thus unidimensional while idealists are multidimensional. This distinction is explained more fully and formally in an [earlier post](/posts/ideal-calibration/).

Decision theory almost insists on the unidimensional view. If we accept a certain set of very plausible axioms about our preferences over [lotteries]{.noted}[^lotteries] of possible worlds, the [Von Neumann-Morgenstern utility theorem](https://en.wikipedia.org/wiki/Von_Neumann%E2%80%93Morgenstern_utility_theorem) (there's an alternate explanation of the theorem along with an interactive calculator in a [previous post](posts/construct-vnm-utility-function-explained/)) shows that there exists a utility function $u$ assigning real numbers to each outcome and rational agents will act as if they are maximizing the expected value of this function. Central to our point here is that the only decision-relevant dimension is the numbers that $u$ provides---our procedure is unidimensional. All considerations like the inherent justice of a social world or its proximity to other worlds are bundled into $u$.

Of course, we could argue about the appropriacy of the theorem here and our willingness to accept the axioms in the matter at hand. In particular, I imagine objections to the concept of lotteries over possible worlds. It's not immediately obvious what a lottery consisting of a 20% probability of one world and a 80% probability of another world is. It's not as though we can construct such a lottery by stepping 'outside' the world and picking one or the other world based on the result of a die roll. And there are extensive discussions about the nature of probability and how it ought to be interpreted---on frequentist grounds, as a subject Bayesian, as an objective Bayesian? Despite these difficulties, I'm not the only one to talk about probabilities over worlds. [@gauss2016], for example, says, "[F]or any set C of constraints there is a probability distribution of possible social worlds that might emerge[.]".

The VNM utility theorem doesn't settle the debate unidimensional vs multidimensional debate once and for all---our quibbles about probability may turn out to be more than quibbles. But I think it is the beginnings of a strong argument in favor of the unidimensional view.

## Epistemic risk aversion

[@gaus2016] highlights another consideration in support of non-ideal theory, the 'Neighborhood Constraint'. The Neighborhood Constraint arises from the fact that "we have far better information about the realization of justice [in worlds similar to the status quo] than in far-flung social worlds". From this fact (which I don't dispute), he infers that we ought to focus on adjacent worlds and ignore the ideal (which I do dispute).

I think this attitude is a good exemplar of epistemic risk aversion, a topic in decision theory. The simplest form of [risk aversion](https://en.wikipedia.org/wiki/Risk_aversion) is actuarial or aleatoric risk aversion. Someone exhibits this kind of risk aversion when they decline a fair bet. We can easily reconcile aleatoric risk aversion with rationality by remembering that utility in money can be [concave](https://en.wikipedia.org/wiki/Concave_function). Epistemic risk aversion or ambiguity aversion is a bit more complicated. It arises when the uncertainty about outcomes is a feature of our minds, not the world---the map, not the territory. It's this second form of risk aversion which make the Neighborhood Constraint a constraint and not the Neighborhood Observation.

Making the connection between the ideal theory debate and decision theory here allows us to apply useful insights from the discussion in decision theory. In particular, we'll start by acknowledging that the dominant decision rule in decision theory is that of maximizing expected value. Many are reluctant to treat epistemic risk differently from aleatory risk and would thus simply maximize expected utility in the social engineering problem, even in the face of uncertainty. This directly contravenes the Neighborhood Constraint. Even if you wanted to treat aleatory and epistemic risk differently, you probably wouldn't come up with a rule that recommends you ignore any action that's highly uncertain a la the Neighborhood Constraint. Instead, you might settle on something like maximin over expected utility as described in [@gardenfors1982].

Decision theory then tends to argue against the Neighborhood Constraint as a constraint and proposes some alternative decision rules.

## Sequential decision theory

A final problem with ideal theory that [@gaus2016] highlights is 'The Choice': "In cases where there is a clear social optimum within our neighborhood that requires movement away from our understanding of the ideal, we must choose between relatively certain (perhaps large) local improvements in justice and pursuit of a considerably less certain ideal."

But I'd argue this is a bad framing. Sequential decision theory suggests that we shouldn't evaluate the imminent decision only in terms of its immediate consequences. We must also evaluate how that decision affects future decisions and the whole eventual sequence of outcomes. Part of what makes any given possible world good or bad is its relationship to future possible worlds. As we discussed in [the section on the VNM utility theorem](#von-neumann-morgenstern-utility-theorem), all decision relevant factors are ultimately combined into a single dimension. If we accept this framing, we can never be confronted with a choice between optimizing on inherent justice and optimizing on proximity to the ideal because these aren't the terms in which we address the problem.

Metaphorically, when Gaus advocates taking the branch of the fork that ignores the ideal in favor of the present, he's advising that you "live every day as if it were your last". The logic of the anti-ideal branch of The Choice would suggest that you always make your plans on Monday with zero regard for how that affects Tuesday. The idealizing branch would suggest that you spend your days journeying through Hell and back in the hopes of spending your final day in Heaven. Obviously, neither of these approaches is right.

When we view The Choice as an ordinary sequential decision problem, it's clear that neither myopia nor hyperopia is correct. We have to integrate our forecasts of the future into our understanding of the present.

# Summary

We started with a tendentious description of the ideal theory debate. After briefly formalizing decision theory, I suggested that the ideal theory debate can be seen as applied decision theory. From this perspective, we turn up easy, compelling answers to the issues of: unidimensional vs multidimensional pursuit of justice, the Neighborhood Constraint, and The Choice.

[^predictive-social]: I'll admit that I'm much more familiar with economics than other social sciences. Nevertheless, some brief searching seems to confirm my impression that other social sciences are much less focused on prediction. Despite Auguste Comte's early declaration that we ought to "Know in order to predict, predict in order to control" and an issue of *Science* devoted to the issue [@jasny2017], misgivings about prediction are both prevalent [@aldridge1999] and dearly held [@flyvbjerg2005] in other corners of the social sciences.
[^lotteries]: In this context, a lottery is a probabilistic mixture of outcomes. A 20% chance of receiving an apple and an 80% chance of receiving a banana constitutes a lottery.

<hr class="references">
