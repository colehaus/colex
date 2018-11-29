---
title: A lazy approach to AI safety
subtitle: Using value of information calculations to defer ethical investigation
published: 2018-03-31
tags: machine ethics, moral uncertainty, constructive
series: Lazy AI safety
---

According to most ethical theories, the actions of any AI agent we create have moral import. To accommodate this reality, we guide the agent with a utility function and (implicitly or explicitly) an ethical framework.

One of the key problems of AI alignment is that we are uncertain about which ethical theory to encode in the agent because we (philosophers, humans, society) are ourselves unsure of the correct ethical theory. How can we expect our agent to act in accordance with our values when we don't even know what our values are?

I propose that we wave the white flag of surrender in the battle to find final, certain answers to the hard problems of ethics. Instead, we should reify our uncertainty and our search procedures in agents we build.

<!--more-->

# Ethical surrender

Our prior should be that "solving" ethics is hard: Many smart people have worked on it for centuries. We can also take a step back and allude to more [fundamental limitations to knowledge](https://plato.stanford.edu/entries/skepticism/) which suggest a definitive solution to ethics isn't around the corner.

There is a certain simplicity to the empirical domain. We can see it, taste it, feel it. And yet, the possibility of certain, empirical knowledge has faced strong skepticism from philosophers for centuries (dating at least to [David Hume](https://plato.stanford.edu/entries/induction-problem/)). Do we simplify the problem of induction by moving to the abstracted domain of ethics? It seems doubtful.

If induction is out, what about deduction? Again, there are [limits](https://plato.stanford.edu/entries/goedel-incompleteness/).

Obviously, this section is brief and handwavey. We've sidestepped big, intricate arguments about the nature of ethics and moral epistemology. But I hope it primes your intuition enough that you're willing to provisionally accept that uncertainty is a major feature of ethics now and in the future.

# Moral uncertainty

Once we accept this uncertainty, we must choose how to respond. If we don't reflect on the idea of moral uncertainty, our approach is likely to approximate "my favorite theory" [@gustafsson2014]. In this approach, we weigh the options, find whichever ethical theory fares best, and discard the rest. That is, if, after analysis, we think the categorical imperative is 20% likely to be true and utilitarianism is 80% likely to be true, we act as utilitarians.

A compelling alternative is to [retain our uncertainty and evaluate actions against a weighted parliament of ethical theories](http://www.overcomingbias.com/2009/01/moral-uncertainty-towards-a-solution.html). In our 80-20 scenario above, any action is evaluated against both theories. If utilitarianism marginally prefers action B to A while the categorical imperative heavily favors A over B, we do A (even though we are "mostly" utilitarian!).

## Moral uncertainty in machines

The impression I have (admittedly, mostly from afar) is that AI alignment has mostly (implicitly) revolved around the "my favorite theory" approach. That is, people have been approaching the issue as deciding which single ethical theory they will encode in an agent. Until they're certain they've decided upon the "one true theory" of ethics, all powerful agents are the stuff of nightmares. I think the parliamentary model improves on this situation.

When encoding the parliamentary model in machines, [there's good reason to avoid simply transferring our own intuitions and perspectives into the machine]{.noted}[^max-ent]. Instead, the parliament's initial distribution should probably be set by a [[maximum entropy distribution](https://en.wikipedia.org/wiki/Maximum_entropy_probability_distribution)]{.noted}[^bootstrap]---that is each ethical theory starts with equal likelihood. Of course, we can't leave it there.

Instead, we will allow and expect our agent to perform [Bayesian updates](https://en.wikipedia.org/wiki/Bayes%27_theorem) to reweight the moral parliament. That is, in addition to any actions it can take in the world (e.g. an autonomous car turning left, a paperclip factory agent reconfiguring its supply chain. For lack of familiarity with a better term, we'll call actions-in-the-world "interventions" henceforth), the agent also always has the option of performing ethical investigation. This supposes that we have a workable answer to the questions of [moral epistemology](https://plato.stanford.edu/entries/moral-epistemology/) and thus a well-founded way to perform these updates. We'll bracket the question of how exactly this can be done while noting that moral epistemology is at least a different hard problem to solve than the problem which AI alignment typically confronts.

### Necessity of agent embedding

The above sounds like a generic algorithm for ethical investigation. Why embed it in an agent rather than asking it to run "to completion" and using the result, or creating a [tool AI](https://www.lesswrong.com/posts/6SGqkCgHuNr7d4yJm/thoughts-on-the-singularity-institute-si)? Under most plausible moral epistemes, I suspect running "to completion" would be [computationally intractable]{.noted}[^intractable]. On the subject of tool AIs, I'll leave it to [Why tool AIs want to be agent AIs](https://www.gwern.net/Tool-AI) and note that foundational ethical investigation seems like a bad place to skimp on capability.

# Arbitrating between interventions and investigations

How should ethical investigation be valued in the agent's utility function? We must answer this question before our agent can make appropriate trade-offs between intervention and ethical investigation. Once we see the ethical investigation as an information-gathering task, the solution falls out naturally. We should use [value of information calculations](https://en.wikipedia.org/wiki/Value_of_information) to value ethical investigation.

Briefly, [value of information](/posts/value-of-information-calculator-explained/) is a well-founded way of quantifying our intuition that uncertainty has a cost. When our actions result in uncertain outcomes, we muddle through as best we can. But information that reduces the uncertainty associated with an action has a tangible value---it may actually cause us to change our actions and obtain better outcomes. If a rational decision maker would pay up to $X for this information, then we say it has a value of $X.

In this case, the value in our value of information calculation is determined by our parliament of moral theories. This is circular (and thus a bit confusing), but, I think, can be [made to work]{.noted}[^circular]. So we'd expect our agent to perform ethical investigation [only insofar as the information produced by that investigation might affect interventions under consideration]{.noted}[^lazy] and where the value of that information is greater than any currently available intervention.

# Claimed benefits

## AI arms race

In an AI arms race, the naive approach to alignment---first, solve ethics; then, develop AGI---puts the most scrupulous developers at a disadvantage [@armstrong2016]. Because the costs of scrupulosity are so high, we expect most developers to end up in the 'unscrupulous' category. The lazy approach may offer a significant advantage here. Because the approach is conceptually straightforward, implementation could be relatively manageable. As such, asking all agent creators to include it is a more plausible request than demanding the cessation of all agent development. Furthermore, when an agent finds itself doing trivial actions of no moral import, it can remain fairly disinterested in ethics. Broad approximations of ethical truth suffice. This means agent creators working in certain fields can be fairly confident that the run time costs---in terms of agent performance overhead, constraints on agent actions, agent predictability, etc.---are minimal. Again, this makes ethical consideration cheaper and more likely.

## Ambiguity alignment

All the moral conundrums that we humans confront are now moral conundrums for our agent as well. When it is faced with truly difficult and important moral decisions, rather than blithely running ahead, our agent will be prompted to pause and refine its ethical views. We can even imagine moral epistemes in which human intuition is a vital input so our agent would actively seek human advice precisely when we are most afraid of alien intelligence.

[^bootstrap]: Beyond just determining the weights of theories, which theories we include at all is crucially important. If we omit the "one true theory" of ethics (thereby implicitly assigning it a credence of 0), tragedy looms. More on this in a later post. <!-- TODO -->
[^max-ent]: I hope to explore and explain this more later in its own post. <!-- TODO -->
[^intractable]: This demands further defense in some future post. <!-- TODO -->
[^circular]: Future post. <!-- TODO -->
[^lazy]: Hence the "lazy" in our title. In particular, we're appealing to the concept of [lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation)---values (both computational and ethical) ought to be computed only on an as needed basis instead of eagerly and preemptively.

<hr class="references">
