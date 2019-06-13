---
title: Causal graphs I
date: 2019-06-09
tags: causality, interactive
js: causal-graphs
css: causal-graphs
---

models and variables
graphical representation: variables as vertices, causal relationships as directed edges
acyclic

path: undirected

# Causal triplets

Three vertices and two edges can be configured in one of [three ways]{.noted}[^why-three]. We give a name to each triplet and to the center node in each triplet. 

Chains and traverses
:   Chains are the most straightforward. If A causes B and B causes C (A → B → C), [then A causes C]{.noted}[^intransitive]. We call the central vertex B a mediator or a traverse. For example, if smoking causes (increased risk of) cancer and cancer causes (increased risk of) death, then smoking causes (increased risk of) death.

Forks and common causes
:   The next possible triplet configuration is what we call a fork. If B causes both A and C (A ← B → C), then A and C will not be independent in light of their common cause. For example, if smoking causes both yellowed fingers [TODO] and lung cancer, we'd expect lung cancer and yellowed fingers to be correlated.

Inverted forks and colliders
:  The final possible triplet configuration is what we call an inverted fork. If A causes B and C causes B (A → B ← C), then A and C will be independent. We call the central vertex B a collider. For example, if smoking causes lung cancer and exposure to high doses of radiation also causes lung cancer, we wouldn't expect smoking and exposure to high doses of radiation to be correlated.

<figure class="big-fig">
<figcaption>Types of causal triplets</figcaption>
| Name of triplet | Name of central vertex  | Diagram   | Ends (A and C) dependent? |
|:----------------|:------------------------|:----------|:--------------------------|
| Chain           | Mediator/Traverse       | A → B → C | Causally (probably)       |
| Fork            | Confounder/Common cause | A ← B → C | Noncausally               |
| Inverted fork   | Collider                | A → B ← C | No                        |
</figure>

D-separated

When we say that a pair of nodes are d-separated, we mean that the variables they represent are definitely independent; when we say that a pair of nodes are d-connected, we mean that they are possibly, or most likely, dependent. 1

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

<div id="nodes-and-seps">
```{=html}
<textarea id="nodes">
- a
- b
- c
- d
</textarea>
```
```{=html}
<textarea id="d-separations">
- [d, c]
- [a, d]
</textarea>
```
</div>
<div id="compatible-graphs"></div>
<div id="d-separation-error"></div>

Back-door path. A noncausal path that connects the independent variable of interest with the dependent variable of interest.

Back-door paths: All paths that start with an arrow pointing to the independent variable and end with an arrow pointing to the dependent variable

Blocked path. A path that contains (a) a collider that the analysis has not been conditioned on or (b) a noncollider (confounder or mediator) that the analysis has been conditioned on. A blocked path does not transmit an association between variables. A path that is not blocked is unblocked, or open, and can transmit an association.

<hr class="references">

[^why-three]: Why three? We have two slots for a directed edge and in each slot an edge can point one of two ways. That creates four options (A ← B ← C; A → B → C; A ← B → C; A → B ← C) but the first two are symmetrical so we don't bother to distinguish between them.
[^intransitive]: See [TODO] and "intransitive dependence" for the rare cases where this doesn't apply.
