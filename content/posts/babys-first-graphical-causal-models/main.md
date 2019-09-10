---
title: Baby's first graphical causal models
date: 2019-06-13
tags: causality, interactive
js: causal-graphs
css: causal-graphs
series: Graphical causal models
include-toc: true
---

# Causal graphs

We can represent causal models as directed [graphs](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)). The vertices in the graph represent different random variables---causes and effects---and the edges represent causal relationships. If two vertices do not have an edge between them, there is no direct causal relationship between them. For example:

![Smoking is causally associated with both lung cancer and yellow fingers](/images/smoking-graph.svg){#smoking-graph}

Some technical details:

- These graphs must be [acyclic](https://en.wikipedia.org/wiki/Directed_acyclic_graph). In a strict sense, something can't be both a cause and an effect of something else. Thing A at time 1 can effect thing B at time 2 which affects thing A at time 3. Causation only flows forward in time and time is acyclic.
- A [path](https://en.wikipedia.org/wiki/Path_(graph_theory)) on a directed graph is a sequence of edges joining a sequence of vertices. We can ignore direction of the edges when forming a path.

# Causal triplets

Now that we've presented the basic idea of modeling causal systems with graphs, we can start to use graphs as a tool to analyze causal models. We'll start by looking at the smallest interesting part of a graph---a triplet consisting of three vertices and two edges. Such a triplet can be configured in one of [three ways]{.noted}[^why-three]. We give a name to each triplet and to the center vertex in each triplet. 

[Chains](#key-terms)
:   Chains are the most straightforward. If A causes B and B causes C (A → B → C), [then A causes C]{.noted}[^intransitive]. We call the central vertex B a mediator or a traverse. For example, if smoking causes (increased risk of) cancer and cancer causes (increased risk of) death, then smoking causes (increased risk of) death.

[Forks](#key-terms)
:   The next possible triplet configuration is what we call a fork. If B causes both A and C (A ← B → C), then A and C will not be independent in light of their common cause. For example, if smoking causes both yellowed fingers and lung cancer, we'd expect lung cancer and yellowed fingers to be correlated.

[Inverted forks](#key-terms)
:  The final possible triplet configuration is what we call an inverted fork. If A causes B and C causes B (A → B ← C), then A and C will be independent. We call the central vertex B a [collider](#key-terms). For example, if smoking causes lung cancer and exposure to high doses of radiation also causes lung cancer, we wouldn't expect smoking and exposure to high doses of radiation to be correlated.

<figure class="triplets-table">
<figcaption>Types of causal triplets</figcaption>
| Name of triplet | Name of central vertex  | Diagram   | Ends (A and C) dependent? |
|:----------------|:------------------------|:----------|:--------------------------|
| Chain           | Mediator/Traverse       | A → B → C | Causally (probably)       |
| Fork            | Confounder/Common cause | A ← B → C | Noncausally               |
| Inverted fork   | Collider/Common effect  | A → B ← C | No                        |
</figure>

So we can determine the causal and non-causal dependence between three factors by turning them into a causal graph and looking at the configuration of the edges.

<!--more-->

# d-separation and d-connection along a path

But that's not terribly useful in a world overflowing with causes, effects and connections. We need to be able to work with bigger graphs. Our next step on that route is to look at arbitrary paths and determine their dependence. The terms used for this are [d-separation](#key-terms) and [d-connection](#key-terms). "When we say that a pair of nodes are d-separated, we mean that the variables they represent are definitely independent; when we say that a pair of nodes are d-connected, we mean that they are possibly, or most likely, [dependent]{.noted}[^d]." [@pearl2016causal]

Two vertices on a path are d-connected if they have no colliders between them. So A and D are d-connected in each of: 

- A → B → C → D
- A ← B → C → D
- A ← B ← C → D
- A ← B ← C ← D 

A and D are d-separated in each of: 

- A → B ← C ← D
- A → B ← C → D
- A → B → C ← D
- A ← B → C ← D

I hope the intuition behind this is clear as a fairly straightforward extension of the logic explained with causal triplets.

We also call a path with a collider on it a [blocked path](#key-terms).

# d-separation and d-connection on graphs

But linear paths still aren't that useful. It's only when we get to full arbitrary directed acyclic graphs that we start to be able to make interesting claims.

In arbitrary graphs, we say that any two vertices are d-connected if they have an undirected path between them which is not blocked (i.e. does not have a collider). If there are no such unblocked paths (i.e. there are no paths at all or all paths have a collider), the two vertices are d-separated.

## Interactive

To get more of a feeling for these terms, you can fiddle with the widget below. 

In the top text area, you can specify a graph as a series of vertices with the edges they point to. So the starting text should be read as "a points to b. b points to nothing. c points to a and b. d points to b". The graph rendered next to it to help you visualize should update once you defocus the text area.

Below the text area, you can ask whether and how any two nodes are d-connected. If they are d-connected, the connecting paths will be highlighted and the paths will be listed.

Finally, the full list of d-separations is always displayed for the current graph.

<hr id="widget-hr">

<div id="spec-and-render">
```{=html}
<textarea id="graph-spec">
a:
  - b
b:
  []
c:
  - a
  - b
d:
  - b
</textarea>
```
<div id="graph-svg"></div>
</div>

<div id="graph-error"></div>

<div class="analysis-panel">
<div class="analysis-header">D-separations:</div>
<div id="d-separation-results"></div>
</div>

<div class="analysis-panel">
<div class="analysis-header">How are <input id="d-connection-from" type="text"></input> and <input id="d-connection-to" type="text"></input> d-connected?</div>
<div id="d-connection-result"></div>
</div>

# Aside

There are several other fairly effective, fairly short introductions to causal graphical models if this one isn't doing it for you:

- [If correlation doesn't imply causation, then what does?](http://www.michaelnielsen.org/ddi/if-correlation-doesnt-imply-causation-then-what-does/)
- [@rohrer2018thinking]

<hr class="references">

[^why-three]: Why three? We have two slots for a directed edge and in each slot an edge can point one of two ways. That creates four options (A ← B ← C; A → B → C; A ← B → C; A → B ← C), but the first two are symmetrical so we don't bother to distinguish between them.
[^intransitive]: See [@pearl2016causal] and "intransitive dependence" for the rare cases where this doesn't apply.
[^d]: The 'd' in d-separated and d-connected stands for "directional" according to most people. But at least [one place](https://www.andrew.cmu.edu/user/scheines/tutor/d-sep.html) says it stands for "dependence" which I think is much more intuitive.
