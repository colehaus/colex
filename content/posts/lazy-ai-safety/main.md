---
title: A lazy approach to AI safety
subtitle: Using value of information calculations to defer ethical investigation
published: 2018-03-31
tags: machine ethics, moral uncertainty
---

According to most ethical theories, the actions of any AI agent we create have moral import. To accommodate this reality, we guide the agent with a utility function and (implicitly or explicitly) an ethical framework.

One of the key problems of AI safety is that we are uncertain about which ethical theory to encode in the agent because we (philosophers, humans, society) are ourselves unsure of the correct ethical theory. How can we expect our agent to act in accordance with our values when we don't even know what our values are?

I propose that we wave the white flag of surrender in the battle to find final, certain answers to the hard problems of ethics. Instead, we should reify our uncertainty and our search procedures in agents we build.

<!--more-->

# Surrender

Our prior should be that "solving" ethics is hard: Many smart people have worked on it for centuries. We can also take a step back and allude to more [fundamental limitations to knowledge](https://plato.stanford.edu/entries/skepticism/) which suggest a definitive solution to ethics isn't around the corner.

There is a certain simplicity to the empirical domain. We can see it, taste it, feel it. And yet, the possibility of certain, empirical knowledge has faced strong skepticism from philosophers for centuries (dating at least to [David Hume](https://plato.stanford.edu/entries/induction-problem/)). Do we simplify the problem of induction by moving to the abstracted domain of ethics? I think not.

If induction is out, what about deduction? Again, there are [limits](https://plato.stanford.edu/entries/goedel-incompleteness/).

Obviously, this section is brief and handwavey. We've sidestepped big, intricate arguments about the nature of ethics and moral epistemology. But I hope it primes your intuition enough that you're willing to provisionally accept that uncertainty is a major feature of ethics now and in the future.

# Moral uncertainty

Once we accept this uncertainty, we must choose how to respond. If we don't reflect on the idea of moral uncertainty, our approach is likely to approximate ["my favorite theory"](http://johanegustafsson.net/papers/in-defence-of-my-favourite-theory.pdf). In this approach, we weigh the options, find whichever ethical theory fares best, and discard the rest. That is, if, after analysis, we think the categorical imperative is 20% likely to be true and utilitarianism is 80% likely to be true, we act as utilitarians.

A compelling alternative is to [retain our uncertainty and evaluate actions against a weighted parliament of ethical theories](http://www.overcomingbias.com/2009/01/moral-uncertainty-towards-a-solution.html). In our 80-20 scenario above, any action is evaluated against both theories. If utilitarianism is indifferent between actions A and B while the categorical imperative heavily favors A over B, we do A (even though we are "mostly" utilitarian!).

# Moral uncertainty in machines

The impression I have (admittedly, mostly from afar) is that AI safety has mostly (implicitly) revolved around the "my favorite theory" approach. That is, people have been approaching the issue as deciding which single ethical theory they will encode in an agent. I think the parliamentary model is a significant improvement.

When encoding the parliamentary model in machines, there's good reason to avoid simply transferring our own intuitions and perspectives into the machine. Instead, the parliament's initial distribution should probably be set by a [maximum entropy distribution](https://en.wikipedia.org/wiki/Maximum_entropy_probability_distribution). Of course, we can't leave it there. That would throw away valuable information about the truth of the moral world (i.e. what evidence do we have bearing on the relative likelihood of each moral theory to be correct). An abhorrence for discarding information is precisely what motivated us to choose the parliamentary approach in the first place.

Instead, we will allow and expect our agent to perform [Bayesian updates](https://en.wikipedia.org/wiki/Bayes%27_theorem) to reweight the moral parliament. That is, in addition to any actions it can take in the world, the agent also always has the option of performing ethical investigation. This supposes that we have a provisional answer to the question of [moral epistemology](https://plato.stanford.edu/entries/moral-epistemology/) and thus a well-founded way to perform these updates. We'll bracket the question of how exactly this can be done while noting that moral epistemology is at least a different hard problem to solve than the problem which AI safety typically confronts.


# Arbitrating between actions and ethical investigation

Now that the agent has an additional action that it must always consider (ethical investigation), we must decide how to value this action so that it can be compared against more standard actions in the world by the agent's utility function. The approach I propose here is to use [value of information calculations](https://en.wikipedia.org/wiki/Value_of_information).

Finally, we really begin to see the benefits of this approach. When an agent finds itself doing trivial actions of no moral import, it can remain fairly disinterested in ethics. Broad approximations of ethical truth suffice. On the other hand though, all the moral paradoxes that we humans confront are now moral paradoxes for our agent as well. When it is faced with truly difficult moral decisions, rather than blithely running ahead, our agent will be prompted to pause and refine its ethical views.

# Future work

We already alluded to the fact that this approach requires some serious thinking about moral epistemology.

Furthermore, the bootstrapping process (which theories do we initially include?) is very important and demands further examination.
