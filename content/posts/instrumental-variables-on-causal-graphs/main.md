---
title: Instrumental variables on causal graphs
date: 2019-06-20
tags: causality, interactive
series: Graphical causal models
js: causal-graphs
css: causal-graphs
---

[Last time](/posts/flip-it-reverse-it-graphical-causal-models/) we talked about viewing d-separation as a tool for model selection. But we're pretty limited in the causal models we can distinguish between by only observing our variables of interest---any two graphs with the same set of d-separations are indistinguishable. [Instrumental variables](https://en.wikipedia.org/wiki/Instrumental_variables_estimation) are a common tool for trying to get around the limitations of purely observational data. 

# Instrumental variables

Instrumental variables (IV) are variables that we're not intrinsically interested in but that we look at in an attempt to suss out causality. The instrument must be correlated with our potential cause, but its only impact on the effect should be via the potential cause. 

The classic example is about---you guessed it---smoking. Because running an RCT on smoking is ethically verboten, we're limited to observational data. How can we determine if smoking causes lung cancer from observational data alone? An instrumental variable! To reiterate, we want a factor that affects smoking prevalence but (almost certainly) does not affect lung cancer otherwise. Finding an instrument that satisfies the <abbr title="instrumental variable">IV</abbr> criteria generally seems to require substantial creativity. Can you think of an instrument for the causal effect of smoking on lung cancer?

...

An instrument that meets these criteria is a tax on cigarettes. We expect smoking to decrease as taxes increase, but it seems hard to imagine a cigarette tax otherwise having an effect on lung cancer.

# Instrumental variables on causal graphs

Okay, so that's what <abbr title="instrument variable">IV</abbr>s are at a high level. But what are they concretely in the graphical causal model setting we've been developing?

## A brief notational interlude

We'll get this out of the way here:

- $\perp\!\!\!\perp$ is the symbol for d-separation
- Once we add the strikethrough, $\not\!\!{\perp\!\!\!\perp}$ mean d-connected.
- If $G$ is a graph, $G_{\overline{X}}$, is $G$ in which [all the edges pointing to X have been removed]{.noted}[^knife]. 

## Defined 

We'll start with the definition and then try to build up a feel for it. An instrumental variable X for the causal effect of Y on Z in graph G must be:

1. d-connected to our cause Y---$(X \not\!\!{\perp\!\!\!\perp} Y)_G$
2. d-separated from our effect Z after severing the cause Y from all its parents---$(X \perp\!\!\!\perp Z)_{G_\overline{Y}}$

<!--more-->

## Interactive

Below is a widget for finding instrumental variables. You can specify your graph (same format as before) in the top text area and make a query about a particular causal relationship in the input fields below the text area. The analysis will update when you defocus the inputs or text area.

Hopefully, you can get an intuition for what <abbr title="instrumental variable">IV</abbr>s mean graphically by generating lots of examples for yourself.

<hr id="widget-hr">

<div id="spec-and-render">
```{=html}
<textarea id="graph-spec">
a:
  - b
  - c
b:
  - c
c:
  []
d:
  - b
</textarea>
```
<div id="graph-svg"></div>
</div>

<div id="graph-error"></div>

<div class="analysis-panel">
<div class="analysis-header">
What are the instruments for the causal effect of <input id="instruments-cause" type="text" value="b" /> on <input id="instruments-effect" type="text" value="c" />?
</div>
<div id="instruments-result"></div>
</div>

## Explained

How does X help us determine whether Y and Z are causally linked? We can analyze things by cases. Y and Z must be d-connected or we wouldn't even be wondering whether X causes Y. Given that they're d-connected, the path between them (we'll assume there's only for the moment) doesn't have any colliders. Instead, the path [can]{.noted}[^models]:

1. Be unidirectional from Y to Z (Y → A → ... → B → Z) which means Y indeed causes Z
2. Be unidirectional from Z to Y (Y ← A ← ... ← B ← Z) which means Z actually causes Y
3. Involve a fork between Y and Z (Y ← ... ← A → ... → Z) which means Y and Z are both caused by some unknown factor

There's only an <abbr title="instrumental variable">IV</abbr> for the causal effect of Y on Z if Y indeed causes Z so we'll figure out how to [add the <abbr title="instrumental variable">IV</abbr>]{.noted}[^adjacent] to the graph by looking at path 1. Our instrument X must be a parent of Y (X → Y → A → ... → B → Z). If it were a child of Y (X ← Y → A → ... → B → Z), it would satisfy <abbr title="instrumental variable">IV</abbr> condition 1 (d-connection to the potential cause), but it wouldn't satisfy <abbr title="instrumental variable">IV</abbr> condition 2 because it would still be d-connected to Z even after Y removed all 0 of the edges from its parents.

Once we make this same modification---add X as parent/cause of Y---to the other paths, we can determine whether X is truly an instrumental variable. In other words, it's important that our instrumental variable separates case 1---where Y genuinely has a causal effect on Z---from the other two cases---where it doesn't. Here's what happens in each case:

1. X → Y → A → ... → B → Z: Our instrument X is d-connected to the effect Z.
2. X → Y ← A ← ... ← B ← Z: Our instrument X is d-separated from Z by the collider at Y..
3. X → Y ← ... ← A → ... → Z: Our instrument X is d-separated from Z by the collider at Y.

Hurray! Our instrumental variable has done just what we wanted---used observation alone to suss out causality. If the potential instrument is d-connected to the potential effect (which can be determined just from the data), the potential cause is actually a cause. If the potential instrument is d-separated from the potential effect (which can be determined just from the data), it turns out that it's not actually an instrumental variable because the potential cause isn't actually a cause of what we thought was an effect.

## As model selection

Last time, we talked about d-separation as a tool for model selection. We can also think of instrumental variables in this way. Instrumental variables are just another tool in the toolbox that allow us to improve our powers of discrimination---allow us to distinguish between models that are indistinguishable when looking only at observations on variables of intrinsic interest.

Below, enter specifications for two causal graphs. The resulting analysis will show you all the instruments that would allow you to distinguish between the two models with observation alone.

<hr id="widget-hr">

<div id="spec1-spec2">
```{=html}
<textarea id="graph-spec1">
a:
  - b
  - c
b:
  []
c:
  []
</textarea>
```

```{=html}
<textarea id="graph-spec2">
a:
  []
b:
  - a
  - c
c:
  []
</textarea>
```
</div>

<div id="discriminate-error"></div>

<div id="discriminate-analysis"></div>

[^adjacent]: For simplicity, we'll only look at instruments that are directly adjacent to our cause Y rather than those that are d-connected at a distance. It doesn't change the analysis materially.
[^knife]: A useful (IMO) mnemonic is to think of the overline is a knife cutting off the edges above---those from parents.
[^models]: The actual fact-of-the-matter path must be one of these, but our analysis so far doesn't tell us which.
