---
title: Value of information calculator
published: 2018-04-14
tags: value of information, decision theory
css: voi-calculator
js: voi-calculator
---

<form id="voi">
<textarea class="voi-tree" id="voi-text">
```{=html}
- outcome:
    finding: coin is double heads
    choices:
    - choice: bet heads
      results:
      - outcome: {label: heads, value: 1}
        prob: 1
      - outcome: {label: tails, value: 0}
        prob: 0
    - choice: bet tails
      results:
      - outcome: {label: heads, value: 0}
        prob: 1
      - outcome: {label: tails, value: 1}
        prob: 0
  prob: 0.5
- outcome:
    finding: coin is double tails
    choices:
    - choice: bet heads
      results:
      - outcome: {label: heads, value: 1}
        prob: 0
      - outcome: {label: tails, value: 0}
        prob: 1
    - choice: bet tails
      results:
      - outcome: {label: heads, value: 0}
        prob: 0
      - outcome: {label: tails, value: 1}
        prob: 1
  prob: 0.5
```
</textarea>
</form>
<output form="voi" for="voi-text">
<div id="voi-error"></div>
<span class="label">Expected value of information</span><span id="voi-result"></span><br/>
<span class="label">Expected value with optimal choices</span><span id="expected-value"></span><br/>
<span class="label">Expected value without information</span><span id="forgotten-expected-value"></span><br/>
<span class="label">Value of perfect information</span><span id="voi-perfect"></span><br/>
<span class="label">Expected value of optimal choices with perfect information</span><span id="perfected-expected-value"></span><br/>
<div class="voi-tree" id="forgotten"><pre></pre></div>
<div class="voi-tree" id="perfected"><pre></pre></div>
</textarea>
</output>
