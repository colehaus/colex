---
title: Progress and preservation in IDA
date: 2019-12-03
tags: machine learning
---

::: macros
$$
\def\sc#1{\dosc#1\csod}
\def\dosc#1#2\csod{{\rm #1{\small #2}}}
$$
:::

[Iterated distillation and amplification](https://arxiv.org/pdf/1810.08575.pdf) (henceforth IDA) is a proposal for improving the capability of human-machine systems to suprahuman levels in complex domains where even evaluation of system outputs may be beyond unaugmented human capabilities. For a detailed explanation of the mechanics, I'll refer you to the original paper just linked, section 0 of [Machine Learning Projects for Iterated Distillation and Amplification](https://owainevans.github.io/pdfs/evans_ida_projects.pdf), or one of the many other explanations floating around the Web. 

We can [view IDA as dynamic programming with function approximation]{.noted}[^substructure] instead of a tabular cache. Just like the cache in dynamic programming, the machine learning component of IDA is a performance optimization. We can excise it and look at just the divide-and-conquer aspect of IDA in our analysis. Then this simplified IDA roughly consists of: (1) repeatedly decomposing tasks into simpler subtasks; (2) eventually completing sufficiently simple subtasks; and (3) aggregating outputs from subtasks into an output which completes the original, undecomposed task. We'll [examine this simplified model]{.noted}[^simple] in the rest of the post. (If you'd like a more concrete description of the divide-and-conquer component of IDA, there's a runnable Haskell demo [here](https://github.com/colehaus/ida-schemes).)

[^simple]: This should be okay because function approximation only makes the problems of progress and preservation harder.
[^substructure]: Asking whether IDA problems have the [optimal substructure](https://en.wikipedia.org/wiki/Optimal_substructure) and [overlapping subproblems](https://en.wikipedia.org/wiki/Overlapping_subproblems) that dynamic programming requires also seems fruitful.

# Safety is progress plus preservation

For type systems, the slogan is "safety is progress plus preservation". Because we're using this only as a cute analogy and organizing framework, we'll not get into the details. But for type systems:

Progress
:   "A well-typed term is [...] either [...] a value or it can take a step according to the evaluation rules."
Preservation
:   "If a well-typed term takes a step of evaluation, then the resulting term is also well typed." 

(Both from [@pierce2002types].)

We also need progress and preservation in IDA. Roughly:

Progress
:   A question is easy enough to be answered directly or can be decomposed into easier subquestions.
Preservation
:   The answer from aggregating subquestion answers is just as good as answering the original question.

Let's try to make this more precise.

<!--more-->

## $\mathrm{progress}$

There are several ways we might interpret "easier". One that seems to have some intuitive appeal is that one question is easier than another if it can be answered with fewer [computational resources]{.noted}[^compute]. 

[^compute]: Of course, "computational resources" is a [leaky abstraction](https://en.wikipedia.org/wiki/Leaky_abstraction).

Regardless, we'll say that we satisfy $\mathrm{progress}_{qa}$ if a question $Q$ is decomposed into subquestions $\mathbf{q}$ such that every subquestion $q$ in $\mathbf{q}$ is not harder than $Q$ and at least one is easier. This is the most obvious thing that IDA is supposed to provide---a way to make hard problems tractable. 

But just noting the existence of such a decomposition isn't enough. We also need to be able to find and carry out such a decomposition more easily than answering the original question. We'll call this property $\mathrm{progress}_{\downarrow}$. $\mathrm{progress}_{\uparrow}$ demands that we be able to find and carry out an aggregation of subquestion answers that's easier than answering the original question. 

Each of these three properties is necessary but they are [not even jointly sufficient for progress]{.noted}[^sufficient]---it could be the case that each of decomposition, answering and aggregation is easier than answering the original question but that all three together are not.

[^sufficient]: If we settled on a precise notion of "easier", we could specify what would be sufficient. For example, if difficulty just adds, the overall $\mathrm{progress}$ requirement would be that the sum of difficulties from decomposition, aggregation and answering is no more than the difficulty from answering the original question in other ways.

We can also view this graphically. In the figure below representing a single step of decomposition and aggregation, we want it to be the case that the computation represented by the arrow from original $Q_0$ to corresponding answer $A_0$ is harder than any of the computations represented by the other arrows.

<figure>
![A graph showing the relationships between original and decomposed questions and answers.](/images/ida/progress.svg)
<figcaption>$\mathrm{progress}_{qa}$, $\mathrm{progress}_{\downarrow}$ and $\mathrm{progress}_{\uparrow}$ mean that the top arrow from $Q_0$ to $A_0$ represents a more difficult computation than each of the bottom, left, and right arrows, respectively.</figcaption>
</figure>

## $\mathrm{preservation}$

There are also several possible interpretations of "as good as". To start with, let's assume it means that one question and answer pair is just as good as another if they have exactly the same denotation. 

We say that a decomposition satisfies $\mathrm{preservation_{\downarrow}}$ if the denotations of $(Q, A)$ and $(Q, \overline{\mathrm{aggregate}}(\overline{\mathrm{answer}}(\mathrm{decompose}(Q))))$ are identical where $(Q, A)$ is a question and answer pair, $\overline{\mathrm{aggregation}}$ is an ideal aggregation, and $\overline{\mathrm{answer}}$ is an ideal answering algorithm. We say that an aggregation satisfies $\mathrm{preservation_{\uparrow}}$ if the denotations of $(Q, A)$ and $(Q, \mathrm{aggregate}(\overline{\mathrm{answer}}(\overline{\mathrm{decompose}}(Q))))$ are identical where $(Q, A)$ is a question and answer pair, $\overline{\mathrm{decompose}}$ is an ideal decomposition, and $\overline{\mathrm{answer}}$ is an ideal answering algorithm. 

Explained differently, $\mathrm{preservation_{\downarrow}}$ requires that the below [diagram commute](https://en.wikipedia.org/wiki/Commutative_diagram) while assuming that answering and aggregation are ideal. $\mathrm{preservation_{\uparrow}}$ requires that the diagram commute while assuming that answering and decomposition are ideal.

<figure>
![A graph showing the relationships between original and decomposed questions and answers.](/images/ida/preservation.svg)
<figcaption>$\mathrm{preservation}_{\downarrow}$ means that the diagram commutes with an ideal bottom and right arrow. $\mathrm{preservation}_{\uparrow}$ mean that the diagram commutes with an ideal bottom and left arrow.</figcaption>
</figure>

## $\sc{PROGRESS}$

$\mathrm{progress}_{qa}$ actually isn't sufficient for our purposes---it could be the case that a series of decompositions produce easier and easier questions but never actually produce questions that are simple enough for a human to answer directly. We name the requirement that our decompositions eventually produce human-answerable subquestions $\sc{PROGRESS}_{qa}$.

## $\sc{PRESERVATION}$

Now let's relax our definition of "as good as" a bit since it's quite demanding. Instead of requiring that the question and answer pairs have exactly the same denotation, we allow some wiggle room. We could do this in a variety of ways including: (1) suppose there is some [metric space](https://en.wikipedia.org/wiki/Metric_space) of [meanings](https://en.wikipedia.org/wiki/Word2vec) and require that the denotations are within $\epsilon$ of each other; (2) require that acting on either question-answer pair produces the same expected utility; (3) require that the utilities produced by acting on each question-answer pair are within $\epsilon$ of each other. For the sake of discussion let's assume something like (1) or (3).

Hopefully, the *mutatis mutandis* for $\mathrm{preservation_{\downarrow}}$ and $\mathrm{preservation_{\uparrow}}$ with this new interpretation of "good enough" is clear enough. (Briefly, the aggregated, answered, decomposition should be within $\epsilon$ of the original answer.)

Unfortunately, the new interpretation means that the single-step (i.e. just one level of decomposition and aggregation) properties are no longer sufficient to guarantee multi-step preservation. It could be the case that each step introduces skew less than $\epsilon$ but that the cumulative skew between the original question and a fully decomposed set of human-answerable questions exceeds $\epsilon$. We'll call the requirement that the series of decompositions maintain skew less than $\epsilon$, $\sc{PRESERVATION_{\downarrow}}$, and that the series of aggregations maintains skew less than $\epsilon$, $\sc{PRESERVATION_{\uparrow}}$.

<figure>
![A graph showing the relationships between original and decomposed questions and answers.](/images/ida/preservation-multi.svg)
<figcaption>$\sc{PRESERVATION}_{\downarrow}$ means that the left hand side of the diagram doesn't break commutativity. $\sc{PRESERVATION}_{\uparrow}$ mean that the right-hand side doesn't break commutativity.</figcaption>
</figure>

# Summary

For every question, there must be a full decomposition to human-answerable questions satisfying $\sc{PROGRESS}_{qa}$ and each decomposed set of questions along the way must satisfy each of $\mathrm{progress}_{qa}$, $\mathrm{progress}_{\downarrow}$, and $\mathrm{progress}_{\uparrow}$. That full decomposition must satisfy $\sc{PRESERVATION_{\downarrow}}$ and the corresponding full aggregation must satisfy $\sc{PRESERVATION_{\uparrow}}$. Each decomposition and aggregation along the way must satisfy $\mathrm{preservation_{\downarrow}}$ and $\mathrm{preservation_{\uparrow}}$.

<figure>
![A graph showing the relationships between original and decomposed questions and answers.](/images/ida/all.svg)
<figcaption>$\mathrm{progress}$ and $\mathrm{preservation}$ properties apply to single steps of decomposition and aggregation. $\sc{PROGRESS}$ and $\sc{PRESERVATION}$ properties apply to repeated decomposition and aggregation.</figcaption>
</figure>

<hr class="references">
