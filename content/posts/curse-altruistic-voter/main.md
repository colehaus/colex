---
title: The curse of the altruistic voter
date: 2018-11-06
tags: voting, mechanism design, constructive
---

Broke
:   Voting on election day
Woke
:   Advocating for alternative voting systems on election day
Bespoke
:   Talking about obscure desiderata of voting systems on election day

# Scenario

## Narrative

Suppose you're voting on increased funding for the local library. You don't personally use the library much, but you figure that others in the polity are reliant on the library. Out of a sense of solidarity you vote for increased funding even though, from a purely egocentric perspective, this reduces your welfare (i.e. the increased cost outweighs your private benefit). When results come in, the library funding measure passes in a landslide and you bask in your altruism.

Alas, the voting results are no guarantee that you've _actually_ acted altruistically. It's entirely possible that you misunderstood the preferences of others and that the polity has made a decision that's net harmful.

## Numerical

For generality, we'll call voting for increased library funding option $\alpha$ and voting against (the status quo) option $\beta$.

<figure>
| Voter | Private value of $\alpha$ | Private value of $\beta$ | Belief about others' private value of $\alpha$ | Belief about others' private value of $\beta$ |
|:------|:--------------------------|:-------------------------|:-----------------------------------------------|:----------------------------------------------|
| A     |                        -1 |                        0 |                                              1 |                                             0 |
| B     |                        -1 |                        0 |                                              1 |                                             0 |
| C     |                        -1 |                        0 |                                              1 |                                             0 |
<figcaption>Individual voters private values for options $\alpha$ and $\beta$ and their belief about others' private values for these options
</figure>

The table outlines a scenario in which each of three voters (it's a small polity) prefers option $\beta$. Unfortunately, they've each come to the inaccurate belief that each other voter prefers option $\alpha$. That is voter A slightly prefers $\beta$ but believes voter B and voter C each slightly prefer $\alpha$.

If our voters are good utilitarians in a [first-past-the-post](https://en.wikipedia.org/wiki/First-past-the-post_voting) system, they'll all vote for $\alpha$ (because $1 + 1 - 1 > 0 + 0 + 0$) and it will win. The resulting [social welfare](https://en.wikipedia.org/wiki/Social_welfare_function) will be $-3 = 3 \cdot -1$. If our voters had voted in a purely egocentric manner---ignoring the preferences of others, they would each pick $\beta$ and the social welfare would have been $0 = 3 \cdot 0$.

This is pretty perverse---our voters have selected the social welfare minimizing option despite their scrupulous motives and they would have better achieved their altruistic ends by voting selfishly!

<!--more-->

# Analysis

What's going on here? The problem arises from [social preferences](https://en.wikipedia.org/wiki/Altruism_theory_of_voting)---second-order preferences about the preference satisfaction of others. In order to incorporate the preferences of others, altruistic voters must (usually implicitly) aggregate them. In other words, it as though altruistic voters are [running their own internal voting system and then voting according to the outcome of that vote]{.noted}[^strategic]. The public, explicit voting system then takes these outputs and aggregates them again. So in any election with altruistic voters, there are really two levels of aggregation happening---the internal aggregations of altruistic voters in which they try to ascertain which option is best for the polity and the external aggregation involving ballots that we usually think of.

The other key ingredient for our perverse outcome is inaccurate beliefs about the preferences of others. If every voter knew precisely the preferences of every other voter and performed a perfect, utilitarian aggregation, they'd all vote the same way and there would be no problem. This, of course, never actually happens. Imperfect information is pervasive.

To recap: When voters have social preferences and imperfect information, they have to work harder (they must perform internal aggregation) and can achieve worse outcomes than if they simply voted egocentrically.

# Relevance

Is all this theorizing just idle fun? Or does this problem arise in practice? I can't say for sure, but here are some indications.

People probably do have social preferences. One argument in favor of this claim is theoretical: it's only rational to vote if you have social preferences [@edlin2007]. People do vote and we like to pretend they're rational, so they must have social preferences. The other argument is more directly empirical. In as survey of 2000 Danes, "29.4% voted for a party they did not believe was best for themselves" [@mahler2017].

It strikes me as almost certain that voters often have incorrect information about other voters. [Pluralistic ignorance](https://en.wikipedia.org/wiki/Pluralistic_ignorance) is a similar phenomenon that's known to exist. See, for example, [the table on divergent support and prevalence of female genital cutting](/posts/norms-wild-clickbait#on-reluctant-female-genital-cutting).

The biggest uncertainty is whether the inaccurate beliefs are of the shape required to produce perverse outcomes. For example, in our initial scenario, if each voter had believed that each other voter only valued $\alpha$ at 0.1 instead of 1, they would have voted for $\beta$ and maximized social welfare ($0.1 + 0.1 - 1 < 0$). Or, two voters could have erroneously thought that everyone else dislikes libraries which would have canceled out the overestimate of the third voters and coincidentally produced the right outcomes.

# Escape

Is there any way to lift the curse?

## Stop being altruistic

One proposal is simply for everyone to vote egocentrically (again, this means they ignore their social preferences and just vote according to their private values). But in a <abbr title="first-past-the-post">FPTP</abbr> system, this is a cure that may well be worse than the disease. While it avoids the perverse outcome in the scenario outlined above, strict adherence ensures [invidious majoritarian tyrannies](/posts/innocuous-invidious-majoritarian-tyrannies/)---the minor preferences of the many outweigh the major preferences of the few.

But it's not quite fair to attribute that problem to social preferences when it's more the fault of a lamentable voting system. Egocentric voting with a [score voting](https://en.wikipedia.org/wiki/Score_voting) system would avoid both the altruist's curse and invidious majoritarian tyrannies.

Mission accomplished! [Hang the banner and board the aircraft carrier](https://en.wikipedia.org/wiki/Mission_Accomplished_speech). Right?

Alas, the problem is not quite solved. Throughout, we have been supposing that the social preferences are strictly utilitarian. As long as the voting system is also formulated on a utilitarian basis like score voting, altruistic voters can simply "delegate" their aggregation to the external aggregation system (i.e. the polity's actual voting system) and vote egocentrically. This works because their internal aggregation algorithm would precisely match the external aggregation algorithm---the voting system already embodies their second-order preferences. 

But most voters aren't strictly utilitarian. They may have other sorts of social preferences. Since they can't rely on the external aggregation mechanism, voters with non-utilitarian social preferences would again be forced to perform internal aggregation in an attempt to better express both their first-order (egocentric) and second-order (social) preferences. As long as different individuals have different types of social preferences (some utilitarian, some [prioritarian](https://en.wikipedia.org/wiki/Prioritarianism), etc.), no voting system can assure all voters that their interests are best served by a simple egocentric vote. Similar ideas are explored in more depth and rigor (in a somewhat different setting) are explored in [@jehiel2001].

## Be a more effective altruist

It appears we can't lift the curse by just ceasing to be altruistic and ignoring our social preferences. The other key cause of the curse we identified was imperfect information. I think there's room for improvement here. Currently, there are polls <i>ad nauseam</i> in the run-up to any election. But the information these polls contain is usually about the way that people will vote, not about their private preferences. As we've examined, people's votes are not identical with their private preferences. If we use the information about voting intentions as a proxy for their private preferences when deciding our votes, we're getting systematically biased information. If pollsters also asked about and published egocentric preferences along with voting intentions, we'd have a much better foundation on which to ground our opinions and votes.

<hr class="references">

[^strategic]: Voting according to social preferences like this is not the same as [tactical voting](https://en.wikipedia.org/wiki/Tactical_voting). One easy demonstration of this fact is to envision how behavior would change if the voter were made a dictator. A tactical voter would change their vote when made a dictator; an altruistic voter would not.
