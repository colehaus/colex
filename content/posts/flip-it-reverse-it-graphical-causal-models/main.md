---
title: Flip it and reverse it
subtitle: Where "it" is the process of going from a graphical causal model to d-separations
date: 2019-06-17
tags: causality, interactive
js: causal-graphs
css: causal-graphs
series: Graphical causal models
---

[Last time](/posts/babys-first-graphical-causal-models/) we talked about causal graphs, what d-separation and d-connection mean, and how to infer these properties from a causal graph. But this isn't terribly useful because it requires that we have a fully specified causal graph. If we're performing research in new or uncertain areas, we have data rather than a causal graph. And this data tells us about d-separations (variables that are independent of each other) and d-connections (variables that are correlated). So our work last time was exactly backwards: graphs to d-separations. This time we'll go from d-separations to graphs.

# Model selection

One way to think about d-separation and d-connection is as helping us with model selection. Last time we presented 

![Smoking is causally associated with both lung cancer and yellow fingers](/images/smoking-graph.svg){#smoking-graph}

as one possible causal model regarding smoking. But it's not the only possibility. We might also be worried that the true causal structure looks like this (just go with it):

![Yellow fingers are independently caused by smoking and lung cancer](/images/smoking-graph-silly.svg){#smoking-graph-silly}

How can we tell them apart? Can we use observational data alone? In this case, observational data alone is enough to distinguish between these two causal models! The key is that the two models have different sets of d-separations. In the original model, all the vertices are d-connected and there are no d-separations (this must be the case since there are no colliders). In the second (silly) model, "smoking" and "lung cancer" are d-separated because "yellow fingers" is a collider between them. If our data show that smoking and lung cancer are independent, we must rule out the first model and prefer the second. If the two variables are correlated, we must rule out the second model and prefer the first.

This is a procedure that works generally: 

1. Draw out the plausible graphical causal models that include all the variables you have data on
2. Determine the d-separations for each plausible model
3. Determine the variables in your data that are independent
4. Retain the models from step 1 whose d-separations in step 2 are compatible with the data analysis in step 3

The ideal is that there's only one model left at the end of step 4. However, it's possible to end up with none. This means that step 1 wasn't permissive enough and more models need to be considered. It's also possible to end up with more than one model. Not all models are distinguishable by observational data alone. This occurs whenever two models have the same set of d-separations.

<!--more-->

# Model generation

We can take this line of thinking even further and do model generation based on d-separations. This lets us go directly from our data to a an exhaustive visualization of all the compatible causal models. The procedure here is:

1. Take a list of variables
2. Generate all possible directed acyclic graphs involving these variables as vertices
3. Take a list of d-separations
4. Retain the models from step 2 that are compatible with the d-separations provided in step 3.

The widget below implements this algorithm. The box on the left takes a list of vertices (in [YAML](https://en.wikipedia.org/wiki/YAML) format). The box on the right takes a list of d-separations where each d-separation is a pair of d-separated vertices like `[x, y]` (YAML again). The updated set of compatible models is rendered below when you defocus the text areas. (If you try to analyze too many vertices at once, the widget will crash because the numbers of possible graphs increases very rapidly and I implemented this in an embarrassingly naive way.)

<hr id="widget-hr">

<div id="nodes-and-seps">
<div>
<div>
Vertices:
</div>
```{=html}
<textarea id="nodes">
- a
- b
- c
- d
</textarea>
```
</div>
<div>
<div>
d-separations:
</div>
```{=html}
<textarea id="d-separations">
- [d, c]
- [a, d]
</textarea>
```
</div>
</div>
<div id="compatible-graphs"></div>
<div id="d-separation-error"></div>
