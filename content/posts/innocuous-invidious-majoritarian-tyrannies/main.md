---
title: Innocuous and invidious majoritarian tyrannies
published: 2018-04-10
tags: social choice, mechanism design
include-toc: true
---

::: macros
$$
\newcommand{\norm}[1]{\lvert #1 \rvert}
$$
:::

<blockquote class="epigraph">
Democracy is two wolves and a lamb voting on what to eat for lunch.
</blockquote>

It seems that, [in common use]{.noted}[^strawman], the term 'tyranny of the majority' conflates two importantly distinct concepts.

# Invidious

The first sense in which one can mean tyranny of the majority is the one highlighted in the epigraph. In this form, some passing whim of the majority overrules and outweighs the critical interest of the minority. The wolves' purely gustatory (pretending that wolves aren't obligate carnivores) interest in the lamb trumps the lamb's literally vital interest in living. Real world examples in this category include: [[Jim Crow laws](https://en.wikipedia.org/wiki/Jim_Crow_laws)]{.noted}[^crow]; [marriage for same-sex couples](https://en.wikipedia.org/wiki/Same-sex_marriage); often [Nimbyism](https://en.wikipedia.org/wiki/NIMBY).

# A shift in perspective

Tyranny of the majority is often framed as a majority violating the political and moral rights of the minority. Once we permit ourselves the ontological spookiness that is 'rights', it seems only fair to allow ourselves [cardinal utility](https://en.wikipedia.org/wiki/Cardinal_utility) and [interpersonal utility comparison](https://en.wikipedia.org/wiki/Social_choice_theory#Interpersonal_utility_comparison). With these tools, we can reframe and make precise the tyranny of the majority described above.

Invidious tyranny of the majority occurs when the weak preferences of many outweigh the strong preferences of the few such that [the actual outcome doesn't maximize utility/satisfaction of preferences]{.noted}[^efficiency].

<!--more-->

For example, the lamb gets -10 utils from being eaten. Each wolf gets 2 utils from eating the lamb. The aggregate net utility of eating the lamb is -6, but their are two votes for and only 1 against. On the other hand, if the wolves were fanatical gourmands that each got 15 utils from eating the lamb, [we wouldn't call their feast a tyranny of the majority]{.noted}[^rights].

In symbols,

$$
d_{v} = \text{argmax}_{d \in D} \sum_{\theta \in \Theta} v(\theta, d) \\
d_{u} = \text{argmax}_{d \in D} \sum_{\theta \in \Theta} u(\theta, d)
$$

where $D$ is the set of all possible outcomes/policies, $\Theta$ is the set of all voters, $v(\theta, d)$ is either [$0$ or $1$]{.noted}[^range] based on whether $\theta$ votes for $d$, and $u(\theta, d)$ is between $0$ and $1$ based on how much utility $\theta$ gets from $d$. Tyranny of the majority occurs when $d_{v}$ and $d_{u}$ disagree even for honest voters (i.e. those that don't try to pursue any strategy but just vote their preferences in the most naive way possible).

# Innocuous

From this perspective, it becomes clearer what in innocuous majoritarian tyranny is. It's when individuals in the majority care about the outcome less than the minority, but honoring the majority's preferences still maximizes aggregate utility. (Presumably/hopefully, no one cries "tyranny" when the majority both has more numbers *and* cares more.)

If you have 10 wolves, each of whom get 2 utils from eating the original lamb, the aggregate utility of eating the lamb is 10 ($10 * 2 - 10 = 10$) versus 0 for abstaining. Thus, eating maximizes overall utility despite the fact that individual preference of each wolf is weaker than the lamb's.

In symbols, an innocuous tyranny is when $d_{v} = d_{u}$, but $\sum_{\theta \in \Theta_{M}} \frac{u(\theta, d_{v})}{\norm{\Theta_{M}}} < \sum_{\theta \in \Theta_{m}} \frac{u(\theta, d')}{\norm{\Theta_{m}}}$ where $\Theta_{M}$ is the set of voters in the majority, $\Theta_{m}$ is the set of voters in the minority, and $d'$ is the minority's preferred outcome.

Obviously, calling this situation innocuous is tendentious and I'm not about run out and slaughter lambs based on this logic. The logic outlined here ignores classic objects to utilitarianism like the 'separateness of persons'. But I think utilitarians at least would be on board with majority rule in 'innocuous' scenario whereas I think zero (?) theories of the good would support the invidious tyranny of the majority.

# Conclusion

To summarize, we call it a 'tyranny of the majority' when the minority has stronger preferences than the majority which aren't honored. This tyranny is invidious if allowing the majority to overrule the minority doesn't maximize aggregate utility. It's innocuous if allowing the majority to overrule the minority does maximize aggregate utility (i.e. the majority is big enough).

[^strawman]: A brief scan of [@mill1869] and [@detocqueville] seems to confirm that they don't plainly emphasize the distinction which follows.
[^crow]: Maybe. [@chin2008] points out that African Americans were an absolute majority in several southern states. It was only via extralegal means that they were disenfranchised.
[^range]: In a typical "one person, one vote" setup. We could easily generalize this to other voting systems.
[^efficiency]: Mechanism design calls this property (your decision rule always chooses the outcome that maximizes utility) *<dfn>efficiency</dfn>*.
[^rights]: At least in this framework where we throw out the concept of rights and instead look only to preferences and utility. Of course, some would reject this framework and say that the violation of the lamb's rights means that this scenario still constitutes a majoritarian tyranny.

<hr class="references">
