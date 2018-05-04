---
title: Build your own Von Neumann–Morgenstern utility function
published: 2018-04-20
tags: decision theory, economics
css: construct-vnm-utility-function
js: construct-vnm-utility-function
---

<form>
<p>Which things would you like to make utility function out of?</p>
<textarea id="goods">
```{=html}
Apple
Banana
Carrot
```
</textarea>
<div class="scenario">
Receive <span class="lottery"><span class="odds">1.00e+0</span> <span class="good">Banana</span> lottery ticket(s)</span><br/><br/>for every <span class="lottery"><span class="odds">1.00e+0</span> <span class="good">Carrot</span> lottery ticket(s)</span>
</div>
<p>Which do you prefer at the given odds?</p>
<div class="buttons">
<input id="first" type="button" value="First"><input id="indifferent" type="button" value="Indifferent"><input id="second" type="button" value="Second">
</div>
</form>
<output>
<div id="function-visualization">
</div>
</output>
