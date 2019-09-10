---
title: Conditioning in causal graphs
date: 2019-07-18
tags: causality, interactive
js: causal-graphs
css: causal-graphs
series: Graphical causal models
include-toc: true
---

As mentioned in the warnings on the [first post on graphical causal models](/posts/babys-first-graphical-causal-models/), I've been lying to you so far. But it was for a good reason: that sweet, sweet expository simplicity. So far, all our definitions, algorithms, etc. have proceeded without any acknowledgment of the social scientists' favorite statistical tool: [[controlling for a variable](https://en.wikipedia.org/wiki/Controlling_for_a_variable)]{.noted}[^conditioning].

In this post, we'll introduce the concept of conditioning to our graphical causal models framework and see how it both complicates things and offers new possibilities. (This post deliberately mirrors the structure of [that one](/posts/babys-first-graphical-causal-models/) so it may be handy to have it open in a second tab/window for comparison purposes.)

# Causal triplets, again

We started out by talking about three types of causal triplets: chains, forks and inverted forks. For convenience, here is the summary table we ended up with:

<figure class="triplets-table">
<figcaption>Types of causal triplets</figcaption>
| Name of triplet | Name of central vertex  | Diagram   | Ends (A and C) dependent? |
|:----------------|:------------------------|:----------|:--------------------------|
| Chain           | Mediator/Traverse       | A → B → C | Causally (probably)       |
| Fork            | Confounder/Common cause | A ← B → C | Noncausally               |
| Inverted fork   | Collider/Common effect  | A → B ← C | No                        |
</figure>

When we add the possibility of conditioning, things change dramatically:

<figure class="triplets-table">
<figcaption>Types of causal triplets with conditioning on central vertex</figcaption>
| Name of triplet   | Name of central vertex    | Diagram     | Ends (A and C) dependent?   |
| :---------------- | :------------------------ | :---------- | :-------------------------- |
| Chain             | Mediator/Traverse         | A → B → C   | No                          |
| Fork              | Confounder/Common cause   | A ← B → C   | No                          |
| Inverted fork     | Collider/common effect    | A → B ← C   | Noncausally                 |
</figure>

The complete reversal of in/dependence occasioned by conditioning on the middle vertex may be a bit surprising. There's a certain reflex that says when ever you want to draw a clean story out of messy data, conditioning on more stuff will help you. But as we see here, [that's not generally true]{.noted}[^open-question].

<!--more-->

## Chains 

Without conditioning, A and C in A → B → C are causally dependent. After conditioning on the mediator/traverse B, they're independent. For example, if smoking causes (increased risk of) lung cancer and lung cancer causes (increased risk of) death, then, without conditioning on lung cancer, death is causally dependent on smoking. However, after conditioning on lung cancer (if we generously assume that there are no other paths from smoking to death), smoking and death are conditionally independent. 
    
The logic here is pretty straightforward but breaking a true causal dependence by conditioning might sometimes happen by accident. In particular, it's most likely to be a problem when the true causal mechanism behind some statistical association is surprising. For example, if we assume that our causal structure is a fork when it's actually a chain, conditioning on B will look the same (i.e. it will break the dependence) even while it hides rather than reveals the causal relationship.

## Forks

Without conditioning A and C in A ← B → C are noncausally dependent. After conditioning on the confounder/common cause B, they're independent. For example, if smoking causes both yellowed fingers and lung cancer, we'd expect lung cancer to be correlated. However, after conditioning on smoking, we'd expect yellowed fingers and lung cancer to be independent. Phew, looks like we can stop worrying about those messy children's art projects.

This is the advertised purpose of conditioning. Controlling for a common cause can get us closer to making correct causal claims.

## Inverted forks

Without conditioning A and C in A → B ← C are independent. After conditioning on the collider/common effect B, they're dependent. For example, if both smoking and exposure to high doses of radiation cause lung cancer, we wouldn't expect smoking and exposure to be correlated in the population at large. However, if we restrict our attention to (condition on) those with lung cancer, we introduce a noncausal dependence---smoking is anticorrelated with radiation exposure.

This is the trickiest of the three. One way to get some intuition for it is to look at the related [Berkson's paradox](https://en.wikipedia.org/wiki/Berkson%27s_paradox). Alternatively, think of an equation like Z = X + Y. If Z is unknown, knowing X tells us nothing about Y. However, if we know that (condition on) Z = 10, we can immediately deduce that Y is 7 when told that X is 3. More generally, it helps to think of conditioning as filtering on the value of the conditioning variable. If we condition on a common effect, any change in one of the causes must be offset by a change in the other cause. This offset creates a statistical dependence between the causes and is necessary if the conditioned variable is to remain unchanged.

It's probably worth pausing and making sure you really understand that conditioning on a collider introduces a statistical dependence because it's key for much of what follows. If the above explanation didn't suffice, one of these might:

- [If correlation doesn't imply causation, then what does?](http://www.michaelnielsen.org/ddi/if-correlation-doesnt-imply-causation-then-what-does/)
- [@rohrer2018thinking]
- [@pearl2016causal]

# d-separation and d-connection along a path

Like before, we'll now move from causal triplets to paths and talk about d-separation and d-connection. Last time we said that two vertices were d-separated along a path between them if there was a collider on the path. Now we modify that condition and say that two vertices are [d-separated](#key-terms) along a path between them if and only if there is an unconditioned collider or conditioned non-collider (i.e. confounder or mediator) in that path. This implies that two vertices are [d-connected](#key-terms) if all vertices along the path between them are some combination of conditioned colliders and unconditioned non-colliders (i.e. confounders and mediators).

We can see how this works out for all paths with four vertices. When we condition on B (the results for C are completely symmetrical), we find that the A and D are d-connected in each of:

- A → B ← C ← D
- A → B ← C → D

and d-separated in each of:

- A → B → C → D
- A ← B ← C ← D
- A → B → C ← D
- A ← B → C ← D
- A ← B → C → D
- A ← B ← C → D

Hopefully, the intuition behind this remains a fairly straightforward extension of the causal triplet logic.

We say that a path is [blocked](#key-terms) (i.e. the endpoints are d-separated) if it has a non-conditioned collider on it or a conditioned non-collider. If we look at the two tables up top, they show that unconditioned colliders and conditioned non-colliders (i.e. mediators and confounders) are the triplets which do not transmit dependence. So it's reassuring to see them showing up here as blockers.

# d-separation and d-connection on graphs

Like before, we'll note that it's only by moving to arbitrary directed acyclic graphs that we start to be able to make interesting claims.

Last time, we said that two vertices on a graph are d-separated if there's no unblocked path between them. We can actually use the same definition here. However, it unfolds differently because---with conditioning in mind---"unblocked" now means something slightly different. Here's how the unfolding works:

1. Vertex A and B are d-separated
2. Vertex A and B have no unblocked paths between them [(from 1 by inlining definition of d-separated)]{.transform-reason}
3. Every path between vertex A and B is blocked (this is satisfied vacuously if A and B have no paths between them) [(from 2 by negation of existential)]{.transform-reason}
4. Every path between vertex A and B has at least one of [(from 3 by inlining definition of blocked)]{.transform-reason}:

    1. a conditioned non-collider (i.e. a mediator or a confounder)
    2. a collider which is not conditioned on and also has no descendants that are conditioned on
    
Hopefully, this mostly makes sense. The only new part is the part where we talk about conditioning on descendants of colliders. 

## Conditioning on descendants of colliders

Just like conditioning on a common effect Y introduces dependence between its causes W and X, conditioning on a descendant Z of that common effect introduces dependence between the common effect's causes W and X. The easiest way to convince yourself of the truth of this claim is to imagine a descendant Z which is perfectly correlated with the common effect Y. In this case, conditioning on Z is as good as conditioning on Y which we already agreed was sufficient to introduce a dependence between W and X.

An example may also help. Let's go back to the example of lung cancer being caused by either smoking or radiation exposure. We already explained that if we restrict our attention to (condition on) those with lung cancer, smoking and radiation exposure become anti-correlated. We would find the same effect if we restricted our attention to those who died of lung cancer (or even just to those who died prematurely).

# Interactive

To get more of a feeling for how all these terms work in the new context with conditioning, you can fiddle with the widget below.

In the top left text area, you can specify a graph as a series of vertices with the edges they point to. So the starting text should be read as "a points to b. b points to nothing. c points to a and b. d points to b". The graph rendered below it should help you visualize should update once you defocus the text area.

In the top right text area, you can specify vertices which ought to be conditioned on.

The full list of d-separations is always displayed for the current graph.

Below the first set of outputs, you can ask whether and how any two nodes are d-connected. If they are d-connected, the connecting paths will be highlighted and the paths will be listed.

<div id="spec-and-conditions">
<div>
<div>
Graph:
</div>
```{=html}
<textarea id="graph-conditioned-spec">
a:
  - b
b:
  - e
c:
  - b
e:
  []
</textarea>
```
</div>
<div>
<div>
Condition on:
</div>
```{=html}
<textarea id="graph-conditioned-on">
- b
</textarea>
```
</div>
</div>
<div id="graph-error"></div>
<div id="graph-and-seps">
<div id="graph-svg"></div>
<div class="analysis-panel">
<div class="analysis-header">D-separations:</div>
<div id="d-separation-results"></div>
</div>
</div>

<div class="analysis-panel">
<div class="analysis-header">How are <input id="d-connection-from" type="text"></input> and <input id="d-connection-to" type="text"></input> d-connected?</div>
<div id="d-connection-result"></div>
</div>

# Postscript

We could also carry on and repeat the other two posts about model selection/generation and instrumental variables with our new understanding of conditioning. But I don't have any plans to do that at the moment. If you really desperately want that content, I guess let me know via [email](mailto:colehaus@cryptolab.net). It's also possible to piece together that functionality fairly straightforwardly using the [library](https://github.com/colehaus/purescript-causal-graphs) underlying the demos already written, if you're an independent sort of person.

<hr class="references">

[^conditioning]: If you're unfamiliar with conditioning as a concept, this might not be the place to learn about it. It probably makes sense to learn about it on its own in some more conventional setting and then see it put to strange and twisted ends here after it has become more familiar. 

[^open-question]: It's something of an open question to me why misconceptions around the role of conditioning aren't more catastrophic. If we suppose that conditioning eliminates confounds and moves us toward causal inference when it sometimes does the opposite, that seems bad. Some resolutions I can imagine:

    - These misconception aren't actually common. People actually practicing science (especially social science) are aware of that conditioning on colliders can introduce dependence. This seems to require more statistical sophistication than is compatible with the replication crisis. It also seems incompatible with Pearl's Cassandra [persona on Twitter](https://twitter.com/yudapearl). I guess a serious attempt at investigating this question would survey textbooks, syllabi, papers, etc to see how frequently they mention the problem of conditioning on colliders.
    - People's intuitions tend to run toward causes. When we see that two variables are correlated and try to come up with a third variable connected to them, we're more likely to imagine a common cause than a common effect.
    - We just got lucky and the causal structure of most things we care to investigate has far more forks than inverted forks.
    - These misconceptions are catastrophic. Controlling for colliders is common and impactful, we/I just haven't noticed.
