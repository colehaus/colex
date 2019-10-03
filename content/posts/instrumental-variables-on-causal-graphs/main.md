---
title: Instrumental variables on causal graphs
date: 2019-06-20
edited: 2019-06-24
tags: causality, interactive
series: Graphical causal models
js: causal-graphs
css: causal-graphs
include-toc: true
---

[Last time](/posts/flip-it-reverse-it-graphical-causal-models/) we talked about viewing d-separation as a tool for model selection. But we're pretty limited in the causal models we can distinguish between by only observing our variables of interest---any two graphs with the same set of d-separations are indistinguishable. [Instrumental variables](https://en.wikipedia.org/wiki/Instrumental_variables_estimation) are a common tool for trying to get around the limitations of purely observational data. 

# Instrumental variables

Instrumental variables (IV) are variables that we're not intrinsically interested in but that we look at in an attempt to suss out causality. The instrument must be correlated with our cause, but its only impact on the effect should be via the cause. 

The classic example is about---you guessed it---smoking. Because running an RCT on smoking is ethically verboten, we're limited to observational data. How can we determine if smoking causes lung cancer from observational data alone? An instrumental variable! To reiterate, we want a factor that affects smoking prevalence but (almost certainly) does not affect lung cancer in other ways. Finding an instrument that satisfies the <abbr title="instrumental variable">IV</abbr> criteria generally seems to require substantial creativity. Can you think of an instrument for the causal effect of smoking on lung cancer?

...

An instrument that meets these criteria is a tax on cigarettes. We expect smoking to decrease as taxes increase, but it seems hard to imagine a cigarette tax otherwise having an effect on lung cancer.

# Instrumental variables on causal graphs

Okay, so that's what <abbr title="instrument variable">IV</abbr>s are at a high level. But what are they concretely in the graphical causal model setting we've been developing?

## A brief notational interlude

We'll get this out of the way here:

- $\perp\!\!\!\perp$ is the symbol for d-separation
- Once we add the strikethrough, $\not\!\!{\perp\!\!\!\perp}$ mean d-connected.
- If $G$ is a graph, $G_{\overline{X}}$, is $G$ in which [all the edges pointing to vertex X have been removed]{.noted}[^knife]. 

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

Unfortunately, I think this may get a bit confusing. Our overall plan is:

1. Enumerate the models compatible with d-connection between the possible cause Y and effect Z
2. Assume that we're right about cause and effect and add a corresponding instrumental variable
3. Calculate d-separations for the models from step 1 with the additional variable from step 2
4. Find that the d-separations now cleanly separate the model in which Y is a cause of Z from others

### Compatible models

How does X help us determine whether Y and Z are causally linked? We can analyze things by cases. The actual [path]{.noted}[^one-path] being modeled must:

1. Be unidirectional from Y to Z (Y → A → ... → B → Z) which means Y indeed causes Z,
2. Be unidirectional from Z to Y (Y ← A ← ... ← B ← Z) which means Z actually causes Y, or
3. Have a fork between Y and Z (Y ← ... ← A → ... → Z) which means Y and Z are both caused by some unknown factor.
4. Not have a collider between Y and Z (Y → ... → A ← ... ← Z). If it did, Y and Z would be d-separated and it would have been immediately obvious from the data that there's no causal relationship.

### The instrumental variable

There's only an <abbr title="instrumental variable">IV</abbr> for the causal effect of Y on Z if Y indeed causes Z so we'll figure out how to [add the <abbr title="instrumental variable">IV</abbr>]{.noted}[^adjacent] to the graph by looking at path 1. Our instrument X must be a parent of Y (X → Y → A → ... → B → Z). If it were a child of Y (X ← Y → A → ... → B → Z), it would satisfy <abbr title="instrumental variable">IV</abbr> condition 1 (d-connection to the potential cause), but it wouldn't satisfy <abbr title="instrumental variable">IV</abbr> condition 2 because it would still be d-connected to Z even after Y removed all 0 of the edges from its parents.

(I suspect the above paragraph reads as very dense. The takeaway is that we want an instrumental variable on path 1 and there's only one way to add a single vertex and edge that satisfies the two <abbr title="instrumental variable">IV</abbr> conditions. That way is for the <abbr title="instrumental variable">IV</abbr> to be a parent of the cause.)

### d-separations

Once we make this same modification---add a variable X which is a parent/cause of Y---to the other paths, we can determine whether X is truly an instrumental variable. In other words, it's important that our instrumental variable separates case 1---where Y genuinely has a causal effect on Z---from the other two cases---where it doesn't. Here's what happens in each case:

1. X → Y → A → ... → B → Z: Our instrument X is d-connected to the effect Z.
2. X → Y ← A ← ... ← B ← Z: Our instrument X is d-separated from Z by the collider at Y.
3. X → Y ← ... ← A → ... → Z: Our instrument X is d-separated from Z by the collider at Y.

### Model selection

Hurray! Our instrumental variable has done just what we wanted---used observation alone to suss out causality. If the random variable X is d-connected to the potential effect (which can be determined just from the data), the potential cause is actually a cause. If the potential instrument is d-separated from the potential effect (which can be determined just from the data), it turns out that it's not actually an instrumental variable because the potential cause isn't actually a cause.

## As model selection

Last time, we talked about d-separation as a tool for model selection. We can also think of instrumental variables in this way. Instrumental variables are just another tool in the toolbox that allow us to improve our powers of discrimination---allow us to distinguish between models that are indistinguishable when looking only at observations on variables of intrinsic interest.

Below, enter specifications for two causal graphs (The two graphs should contain the same set of vertices---only the edges should differ.). The resulting analysis will show you all the instruments that would allow you to distinguish between the two models with observation alone. Each row contains a different instrumental variable. The left column shows the extra variable as it would look on the graph specified in the left-hand text area while the right column shows the <abbr title="instrumental variable">IV</abbr> on the right text area's graph. In each row, you should see that the columns have different sets of d-separations.

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

[^adjacent]: For simplicity, we'll only look at instruments that are directly adjacent to our cause Y rather than those that are d-connected at a distance. [It doesn't change the analysis materially.](https://www.theproofistrivial.com/)
[^knife]: A useful (IMO) mnemonic is to think of the overline as a knife cutting off the edges above the vertex---those from parents.
[^one-path]: We'll assume there's only one path for the sake of expository simplicity. [The story doesn't really change with multiple paths](https://www.theproofistrivial.com/).
