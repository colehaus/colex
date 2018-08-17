---
title: False dichotomies and the ideal theory debate
published: 2018-07-09
tags: ideal theory, decision theory, political philosophy
series: The Tyranny of the Ideal
css: false-dichotomy-ideal-theory-debate
js: false-dichotomy-ideal-theory-debate
---

# Ideal and non-ideal theory

We've already described ideal theory in [previous posts](/posts/utopia-infinitude-secretaries/), but we'll give a short recap here for the sake of self-sufficiency. Ideal theory suggests that when making decisions about alternative social worlds---that is, about different political and economic institutions, we should have an ideally just society in mind. Non-idealists argue that this information is irrelevant; we only need to be able to perform pairwise comparisons. A popular metaphor in the area is that of mountain climbing. In the language of this metaphor, ideal theorists like [John Rawls](https://en.wikipedia.org/wiki/John_Rawls) suggest that mountaineers orient themselves toward Everest while non-idealists like [Amartya Sen](https://en.wikipedia.org/wiki/Amartya_Sen) suggest that knowledge of [Everest](https://en.wikipedia.org/wiki/Mount_Everest) is irrelevant when comparing the heights of [Kilimanjaro](https://en.wikipedia.org/wiki/Mount_Kilimanjaro) and [Denali](https://en.wikipedia.org/wiki/Denali).

# Thesis

I contend this is a debate which can be [dissolved](http://askphilosophers.com/question/5254). There is no necessary opposition between incrementalism and idealism. Instead, all of these perspectives can be ably unified under the framework of decision theory.

# Dichotomy

Before I can make the argument that's it's a *false* dichotomy, I need to show that it's a putative dichotomy. There's little value in attacking [straw men](https://en.wikipedia.org/wiki/Straw_man). Since I've just read [@gaus2016], we'll examine that in detail and expect that it's representative of the larger discussion.

The boundary that Gaus draws is between worlds in the 'neighborhood' of the status quo and those outside it. If we restrict our attention to worlds in the neighborhood, we're engaging in non-ideal theory, but if we speculate on distant worlds we're doing ideal theory. What is this key neighborhood concept? In Gaus's words: "A neighborhood delimits a set of nearby social worlds characterized by relatively similar justice-relevant social structures."

So we're already on firm grounds for a claim of dichotomous thinking. On this view, the [structure of the problem is dichotomous]{.noted}[^relax]. But Gaus also demonstrates the dichotomous view when describing the divergent implications of the ideal and non-ideal view:

<blockquote>[L]ocal optimization often points in a different direction than pursuit of the ideal. We then confront what I have called [The Choice]{#the-choice}: should we turn our back on local optimization and move toward the ideal? [... O]ur judgments within our neighborhood have better warrant than judgments outside of it; if the ideal is outside our current neighborhood, then we are forgoing relatively clear gains in justice for an uncertain prospect that our realistic utopia lies in a different direction. [Mill’s revolutionaries]{.noted}[^revolutionaries], certain of their own wisdom and judgment, were more than willing to commit society to the pursuit of their vision of the ideal; their hubris had terrible costs for many.</blockquote>

# Similarities

Now, I'll hope you agree ideal and non-ideal theory are framed as incompatible. On that assumption, I'll begin to argue against the dichotomy.

## Uncertainty all around

I do accept Gaus's Neigborhood Constraint---our knowledge of distant social words is much less reliable than our knowledge of worlds similar to the status quo. Furthermore, I think we have non-trivial uncertainties about the workings and justice of worlds that *are* nearby. Importantly, (though not, I think, crucially) I don't see any obvious reason for discontinuities in the reliability of our knowledge. My intuition suggests it drops off smoothly with distance from the status quo []{.spark #reliability}.

<!--more-->

## Foresight all around

Non-idealists not only contend that our knowledge of the ideal is highly uncertain, they suggest it's otiose. On the contrary, knowledge about world C is useful for evaluating worlds A and B not only when C is the ideal but when all of A, B, and C are in the same neighborhood. As long as we don't expect the next world we inhabit to be the last world we ever inhabit, a thorough evaluation of that next world should include it's likely effects on the subsequent chain of worlds. That is, an evaluation which judges B against A only on the immediate effects and not on how B enables and forecloses other nearby possibilities like C is a very blinkered analysis indeed. So again, we see a difference of degree rather than kind---we should surely evaluate the knock-on effects of our choices and the only real question is how rapidly does the value of such estimation decline as we step into the future and away from the status quo.

# Decision theory

Finally, the promised unification. If we believe the story so far, key problems in ideal theorizing include making [hard trade-offs](#the-choice), [choice under uncertainty](#uncertainty-all-around), and [intertemporal choice](#foresight-all-around). But these are precisely the problems that decision theory (broadly construed) seeks to address!

For example, decision theory offers fairly straightforward recommendations in Gaus's Choice (between pursuing local improvements and the ideal when they point in opposite directions). If you buy into an expected utility maximizing decision theory, you simply calculate the value of each choice on offer, accounting for uncertainty and risk aversion, and pick the maximal.

And decision theory doesn't recommend that you ignore the ideal just because it's uncertain---it never helps to throw away information. You can incorporate your limited understanding of the ideal and any reasonable decision theory will guard against tyrannies of hubris by weighting certain and speculative knowledge differently.

A final example of the decision theory perspective dissolving these problems: Decision theories can generally accommodate sequences of decisions over time and allow the formulation of optimal strategies. Problems like evaluating worlds in light of the futures they encourage and discourage aren't foreign to decision theory---optimal [multi-armed bandit](https://en.wikipedia.org/wiki/Multi-armed_bandit) algorithms, for example, will sacrifice near-term gains for better long-run outcomes.

(Actually using these techniques well for political philosophizing is difficult. The difficulty of application does little to convince me this is the wrong approach---I'd expect it to be hard to determine an optimal trajectory through possible worlds.)

# A parting metaphor

Suppose you're playing [Texas hold'em](https://en.wikipedia.org/wiki/Texas_hold_%27em) at some louche casino. You've just been dealt your pocket cards. Now, imagine some bon vivant sidles up behind you and advises, "Don't worry about what the river might turn up! It's so far away and uncertain; just think about the flop. Not to mention, optimizing for the river might lead to worse play in the near term." The only proper response is to tell the lush to sober up and take their sloppy heuristics elsewhere. You believe in decision theory, thank you very much.

Obviously, the metaphor isn't perfect. But it's crystal clear you should use decision theory for poker and, at this point, I think the (rebuttable) presumption is we should use it for political philosophizing as well.

<hr class="references">

[^relax]: Though it somewhat undermines my point, I'll note that Gaus says, "For simplicity, I assume that there is a clear boundary between the worlds that are in our neighborhood and those that are too dissimilar for us to make as firm judgments about, though of course this is an idealization (§I.3.3), which we will relax (§II.4.2)." Unfortunately, after several rereadings of §II.4.2, I disagree that he relaxes the idealization.
[^revolutionaries]: <blockquote>It must be acknowledged that those who would play this game on the strength of their own private opinion, unconfirmed as yet by any experimental verification—who would forcibly deprive all who have now a comfortable physical existence of their only present means of preserving it, and would brave the frightful bloodshed and misery that would ensue if the attempt was resisted—must have a serene confidence in their own wisdom on the one hand and a recklessness of other people’s sufferings on the other, which Robespierre and St. Just, hitherto the typical instances of those united attributes, scarcely came up to. [[@mill1879]]{.attribution}</blockquote>
