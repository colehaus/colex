---
title: Causal graphs
date: 2019-06-25
tags: causality, interactive
js: causal-graphs
css: causal-graphs
series: Graphical causal models
---

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
d:
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
<div id="graph-svg"></div>

<div id="graph-error"></div>

<div class="analysis-panel">
<div class="analysis-header">D-separations:</div>
<div id="d-separation-results"></div>
</div>

<div class="analysis-panel">
<div class="analysis-header">How are <input id="d-connection-from" type="text"></input> and <input id="d-connection-to" type="text"></input> d-connected?</div>
<div id="d-connection-result"></div>
</div>

