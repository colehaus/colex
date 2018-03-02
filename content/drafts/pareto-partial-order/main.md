---
title: Pareto improvement as partial order
published: 2018-03-01
tags: economics, metrics
---

We covered the notion of [Pareto improvement](https://en.wikipedia.org/wiki/Pareto_efficiency) in the [preceding post](../pareto-examples/). I briefly alluded to the fact that it's a strict [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set). Let's explore that a bit more.

A strict partial order is a [binary relation](https://en.wikipedia.org/wiki/Binary_relation) that is irreflexive (no element precedes itself), and transitive. Pareto improvement satisfies both of these criteria:

Irreflexive: $\neg (A \prec A)$
:    No scenario is a Pareto improvement over itself because no one strictly prefers it (person 1 doesn't prefer scenario A to scenario A), trivially.

Transitive: $A \prec B, B \prec C \Rightarrow A \prec C$
:    If arbitrary scenario B is a Pareto improvement over arbitrary scenario A and arbitrary scenario C is a Pareto improvement over scenario B, then scenario C is a Pareto improvement over scenario A. In other words, if no one is worse off in scenario B than scenario A and no one is worse off in scenario C than scenario B, then, clearly, no one is worse off in scenario three than scenario A. And if at least one person is better off in scenario B than scenario A and at least one person is better off in scenario C than scenario B, then, clearly, at least one person is better off in scenario C than scenario A.

Alternatively, we can just take the shortcut of noting that Pareto improvement is the [componentwise order](https://en.wikipedia.org/wiki/Product_order) of the total orders of individual preferences.

In the [preceding post](../pareto-examples/), we objected to the fact that Pareto improvement is partial---unable to render judgment between many scenarios and declare one as better than another despite our strongest intuitions. [We could remedy this problem if instead we had a [total order](https://en.wikipedia.org/wiki/Total_order)]{.noted}[^antichain]. We turn a strict partial order into a strict total order by adding the following property:

Trichotomous: $(A \prec B) \lor (B \prec A) \lor (A = B)$
:    Arbitrary scenario A is preferred to arbitrary scenario B or scenario B is preferred to scenario A or scenario A and B are the same.

The question is, how to introduce this property? If we're able to introduce this property while retaining the [thinness](https://plato.stanford.edu/entries/thick-ethical-concepts/) (i.e. value neutrality) of Pareto improvement, that would be a very interesting thing indeed. We would have a tool to judge any and every policy, any and every state of society, any and every counterfactual world while keeping our private values out of the decision. We would, perhaps, have "solved" objective morality.

[^antichain]: You may object that we're making too big a leap. The problem we outlined is only that Pareto improvement is too partial a partial order. We don't necessarily need a total order. It may suffice to have fewer and smaller [antichains](https://en.wikipedia.org/wiki/Antichain). In fact, we could define an "index of totality" for a partially ordered set $P$ as $\frac{\text{no. of antichains in }P}{|P|}$ where $\frac{1}{|P|}$ means all elements in $P$ are incomparable to each other (e.g. $(0, 2)$, $(1, 1)$, and $(2, 0)$ when lifting $\lt$ via the componentwise order) and $\frac{|P|}{|P|}$ means all elements are comparable to each other (e.g. $(0, 0)$, $(1, 1)$ and $(2, 2)$). Thus, an index of totality at $1$ actually indicates a total order. With this notion in mind, instead of demanding a total order, we could instead try to fix the problems of Pareto improvement by demanding some partial order with a sufficiently high index of totality. All that said, we'll set this idea aside for the moment.
