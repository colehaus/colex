---
title: Standalone–Value of information calculator
published: 2018-06-13
tags: value of information, decision theory, interactive
css: value-of-information-calculator
js: value-of-information-calculator
---

# Calculator

The input required by the calculator is a tree in [YAML format](https://en.wikipedia.org/wiki/YAML). The top level of the tree describes the different pieces of information you might find (e.g. the coin is double heads) after investigating and how likely you think each piece of information is. Each of these info nodes has children corresponding to actions you might take (e.g. bet heads). The final level of the tree (the children of the action nodes) describe the outcomes that actually occur and their probability of occurrence given the information received.

In addition to showing the expected value of information, the calculator shows the corresponding simplified scenario in which you have no way of gaining information about outcomes at the bottom. For example, it shows you the original coin flip scenario before your susurrous friend comes along. This is offered simply as a point of comparison.

<!--more-->

## Input

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

## Results

<output form="voi" for="voi-text">
<div id="voi-error"></div>
<div id="result-numbers">
<span class="label">Expected value with information</span><span id="expected-value"></span><br/>
<span class="label">Expected value without information</span><span id="forgotten-expected-value"></span><br/>
<span class="label">Expected value of information</span><span id="voi-result"></span><br/>
</div>
<figure><figcaption>Scenario without information</figcaption><pre class="voi-tree" id="forgotten"></pre></figure>
</output>

<hr class="references">
