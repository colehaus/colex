---
title: Ethics as well-order
published: 2018-04-17
tags: ethics, order theory
css: ethics-well-order
---

Ethics is fundamentally about 'ought'. What ought we to do? Which actions are proscribed and which prescribed? Among all available actions, which should we actually pursue? I think we can formalize this basic understanding and draw some interesting conclusions.

In any given moral decision, we have some nonempty set $\mathcal{A}$ (depending on how we individuate, possibly infinite) of possible actions. But, alas, we cannot perform all these actions; only one. So an ethical theory is something that for every possible $\mathcal{A}$ selects a distinguished element $a \in \mathcal{A}$. That is, a moral theory is a [choice function](https://en.wikipedia.org/wiki/Choice_function) over nonempty sets of actions.

Let's try to think about this choice function in a little more detail. We'll start with the simplest case:

::: single-choice
::: math-col
sets with only a single element. Here, our choice function simply plucks out this single element. In symbols, $\forall a \in \mathcal{A} : f(\{a\}) = a$.
:::

::: ethics-col
ethical choices with only one possible action (i.e. there is not even a choice between action and inaction). Here, [we'd hope our moral theory would recommend the only possible action](https://en.wikipedia.org/wiki/Ought_implies_can).
:::
:::

<!--more-->

On to the next step:

::: double-choice
::: math-col
Now, we want to define our choice function over two element sets as well. In order to relate the two elements in each set, we must talk about a [binary relation](https://en.wikipedia.org/wiki/Binary_relation) that we'll call $\prec$ (read $a \prec b$ as $a$ is worse than $b$ or as $b$ is better than $a$). What properties should this relation have? We'll make our relation [[irreflexive](https://en.wikipedia.org/wiki/Reflexive_relation)]{.noted}[^irreflexive]. It should also be [[asymmetric](https://en.wikipedia.org/wiki/Asymmetric_relation)]{.noted}[^asymmetric]. If we stick to our earlier claim that an ethical theory must pick a distinguished element "for every possible" $\mathcal{A}$ (which, tendentiously, we will for the rest of the post), then we're also requesting [[totality](https://en.wikipedia.org/wiki/Serial_relation)]{.noted}[^totality]. (Totality also demands an [equivalence relation](https://en.wikipedia.org/wiki/Equivalence_relation). We choose the [identity relation](https://en.wikipedia.org/wiki/Equality_(mathematics)) for the remaining discussion.)

So our choice function applies our irreflexive, asymmetric binary relation to our two element set and always chooses the better element of the two.
:::

::: ethics-col
What happens when our moral theory needs to actually start pulling its weight and giving guidance between two different actions (call them A and B)? At its simplest, the theory can either declare that A is better or instead that B is better. Or it might declare that the two actions are incomparable---neither stands anywhere in moral relation to the other. This is often unsatisfactory (as suggested in a [previous post](/posts/pareto-examples)). So we'll provisionally adhere to the introduction's demand that our moral theory never avail itself of this escape hatch. This demand for comprehensive guidance means that we also need a notion of moral equivalence. At a minimum, every action must be morally equivalent to itself. (Our moral theory ought not to claim that action A is better than A.)

All of that is to say, when presented with two different actions, our moral theory always recommends one or the other as the better to pursue.
:::
:::

Now it really starts to get complicated.

::: triple-choice
::: math-col
We've defined our choice function for sets $\mathcal{A}$ of cardinality $1$ and $2$. What about $3$?

::: skippable
(These next several paragraphs are a belaboring of what are presumably widely shared intuitions about transitivity and using a binary relation to pick the greatest element in a set.)

Let's try to build it out of what we already know. For any given $a, b, c \in \mathcal{A}$, if we look at the binary relation as defined on $a ? b$ and $b ? c$, we have 9 cases to consider:

| $a ? b$         | $b ? c$         | $f(\{a,b,c\})$ |
| :-------------- | :-------------- | :-------       |
| $a \prec b$     | $b \prec c$     | $\mathcal{W}$  |
| $a \prec b$     | $b = c$         | $\mathcal{X}$  |
| $a \prec b$     | $c \prec b$     | $\mathcal{Y}$  |
| $a = b$         | $b \prec c$     | $\mathcal{X}$  |
| $a = b$         | $b = c$         | $\mathcal{X}$  |
| $a = b$         | $c \prec b$     | $\mathcal{X}$  |
| $b \prec a$     | $b \prec c$     | $\mathcal{Z}$  |
| $b \prec a$     | $b = c$         | $\mathcal{X}$  |
| $b \prec a$     | $c \prec b$     | $\mathcal{W}$  |

Table: Generic choice function on elements of cardinality 3 based on relationship between $a$ and $b$ and between $b$ and $c$

I've taken the liberty of categorizing the rows. Those in category $\mathcal{W}$ rely on [transitivity]{.noted}[^transitivity] of our binary relation. Those in category $\mathcal{X}$ rely on transitivity of the identity relation. In category $\mathcal{Y}$, $a ? c$ is underdetermined, but each $\prec b$. In category $\mathcal{Z}$, $a ? c$ is again undefined but $b \prec$ each.

Transitivity of the identity relation strikes me as /very/ innocuous. Transitivity of the binary relation also seems very intuitive to me, but some contest it. See, for example, [@rachels1998]. In category $\mathcal{Y}, because our choice function only needs to select the one best element, we don't care that $a ? c$ is undetermined---$b$ is greater than both. It's only in $\mathcal{Z}$, that we must again resort to our binary relation to settle whether element $a$ or $c$ is greater. Putting that all together, we rewrite the table:

| $a ? b$         | $b ? c$         | $f(\{a,b,c\})$ |
| :-------------- | :-------------- | :-------       |
| $a \prec b$     | $b \prec c$     | $c$            |
| $a \prec b$     | $b = c$         | $b/c$          |
| $a \prec b$     | $c \prec b$     | $b$            |
| $a = b$         | $b \prec c$     | $c$            |
| $a = b$         | $b = c$         | $a/b/c$        |
| $a = b$         | $c \prec b$     | $a/b$          |
| $b \prec a$     | $b \prec c$     | $a?c$          |
| $b \prec a$     | $b = c$         | $a$            |
| $b \prec a$     | $c \prec b$     | $a$            |

Table: Choice function on elements of cardinality 3 based on relationship between $a$ and $b$ and between $b$ and $c$ under certain constraining assumptions

:::

That's all a long-winded way of suggesting that we can 'lift' our binary relation to a choice function on sets of cardinality 3 with the help of transitivity.

:::
::: ethics-col
How does our moral theory cope when asked to recommend among three distinct, morally relevant actions (call them A, B, and C)? Let's start by just making pairwise comparisons---we already established a system for this in the prior step. Because we're making sure to consider only distinct actions which are not morally equivalent, there are $2^3 = 8$ possible pairwise combinations.

|   # | A vs. B         | B vs. C         | A vs. C  |
| :-- | :-------------- | :-------------- | :------- |
|   1 | a better        | b better        | a better |
|   2 | a better        | b better        | c better |
|   3 | a better        | c better        | a better |
|   4 | a better        | c better        | c better |
|   5 | b better        | b better        | a better |
|   6 | b better        | b better        | c better |
|   7 | b better        | c better        | a better |
|   8 | b better        | c better        | c better |

Table: Table showing possible results of pairwise comparisons between morally relevant actions A, B, and C.

6 of the 8 (i.e. all but rows 2 and 7 in the table) are perfectly reasonable and take a form like 'A is better than B which is better than C with is the worst.'. However, 2 of the 8 pairwise combinations (i.e. rows 2 and 7 in the table) produce cycles like 'A is better than B which is better than C which is better than A which is better than B' which continues ad nauseum.

Needless to say, permitting cycles in our moral theory has many strange consequences. If we wish to forbid these cycles, the property we must appeal to is [transitivity](https://en.wikipedia.org/wiki/Transitive_relation). Nevertheless, the transitivity of 'better than' is actually an open question in the philosophical community. See, for example, [@rachels1998] for an entry point into that discussion.

Since my intuitions argue in favor of transitivity being important, we'll assume that for the remainder of the post.

Summarizing, we've upgraded our ethical theory to work across sets of three actions by doing pairwise comparisons and aggregating them to find the best option subject to certain reasonableness constraints.
:::
:::

This is quickly becoming tedious. Surely, we can't hope to construct the choice function for each possible cardinality of $\mathcal{A}$ on unto infinity. As it turns out we don't need to. We now have all the tools we need to generalize.

::: general-choice
::: math-col
If we review all the properties we've accumulated on our binary relation, it is (somewhat redundantly) irreflexive, asymmetric, transitive, trichotomous, and total. That is, our binary relation is actually a [strict total order](https://en.wikipedia.org/wiki/Total_order#Strict_total_order). And the choice function we've been implementing so far simply picks the greatest element in each subset of $\mathcal{A}$. We can extend this indefinitely. A total order on $\mathcal{S}$ with the property that every nonempty subset of $\mathcal{S}$ has a greatest element is a (inverted) [well-order](https://en.wikipedia.org/wiki/Well-order). [Every finite total order is also a well-order](https://proofwiki.org/wiki/Finite_Totally_Ordered_Set_is_Well-Ordered). So we can generalize this approach to the choice function to arbitrary subsets of $\mathcal{A}$ of finite cardinality $n$. We leave the issue of infinite $\mathcal{A}$ aside for the moment.
:::
::: ethics-col
We can, in principle, follow this same basic approach to scale our moral theory to choose among an arbitrary number of actions. We just keep making pairwise comparisons until we conclude what the best option is (We're guaranteed a unique answer as long as the pairwise comparisons obey transitivity.).
:::
:::

[^irreflexive]: Irreflexive---$\forall a \in \mathcal{A} : \neg \left(a \prec a\right)$. For example, $\leq$ on the integers is reflexive while $\lt$ is irreflexive.
[^asymmetric]: Asymmetric---$\forall a, b \in \mathcal{A} : a \prec b \implies \neg \left(b \prec a\right)$. For example, it's not true that both $1 < 2$ and $2 < 1$.
[^totality]: Totality---$\forall a, b \in \mathcal{A} : a \prec b \veebar b \prec a \veebar a \approx b$. For any two integers $a$ and $b$, only one of the following holds: $a < b$; $b < b$; $a = b$. You cannot pick two integers which are 'undefined' with respect to $<$ and $=$.
[^transitivity]: Transitivity---$\forall a, b, c \in \mathcal{A} : \left(a \prec b \land b \prec c\right) \implies a \prec c$. For example, $1 < 2$, $2 < 3$, and $1 < 3$.
