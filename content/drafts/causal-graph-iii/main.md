---
title: Causal graphs III
date: 2019-06-09
tags: causality, interactive
js: causal-graphs
css: causal-graphs
---

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
