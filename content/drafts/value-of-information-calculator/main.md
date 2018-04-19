---
title: Value of information calculator
published: 2018-04-14
tags: value of information, decision theory
css: voi-calculator
js: voi-calculator
---

# Input

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

# Results

<output form="voi" for="voi-text">
<div id="voi-error"></div>
<div id="result-numbers">
<span class="label">Expected value with given information</span><span id="expected-value"></span><br/>
<span class="label">Expected value without information</span><span id="forgotten-expected-value"></span><br/>
<span class="label">Expected value of given information</span><span id="voi-result"></span><br/>
<span class="label">Expected value with perfect information</span><span id="perfected-expected-value"></span><br/>
<span class="label">Expected value of perfect information</span><span id="voi-perfect"></span><br/>
</div>
<figure><figcaption>Scenario without information</figcaption><pre class="voi-tree" id="forgotten"></pre></figure>
<figure><figcaption>Scenario with perfect information</figcaption><pre class="voi-tree" id="perfected"></pre></figure>
</textarea>
</output>
