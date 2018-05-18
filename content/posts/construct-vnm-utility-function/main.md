---
title: Standalone–Build your own von Neumann–Morgenstern utility function
published: 2018-05-14
tags: decision theory, economics
css: construct-vnm-utility-function
js: construct-vnm-utility-function
---

<form>
<p>Which things would you like to make a utility function out of?</p>

<!--more-->

<textarea id="goods">
```{=html}
Apple
Banana
Carrot
```
</textarea>
<p>Which do you prefer?</p>
<div class="scenario">
Receive <span id="first-good" class="lottery"><span class="odds">1.00e+0</span> <span class="good">Banana</span> lottery ticket(s)</span><br/><br/>or <span id="second-good" class="lottery"><span class="odds">1.00e+0</span> <span class="good">Carrot</span> lottery ticket(s)</span><br/><br/>
<span id="indifferent" class="lottery">Indifferent</span>
</div>
</form>
<output>
<figure id="function-visualization">
<figcaption>Estimated value of goods relative to favorite good</figcaption>
<div id="function-chart">
</div>
</figure>
</output>
