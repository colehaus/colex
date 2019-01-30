---
title: Priority decisions
series: An Introduction to Decision Theory
published: 2019-01-17
tags: decision theory, interactive, yaas
js: priority-decisions
css: priority-decisions
---

<textarea id="maximin-table" class="decision-table">
```{=html}

|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | a       | y       | z      |
| Action 2 | b       | c       | y      |
| Action 3 | b       | d       | z      |
```
</textarea>

::: {#maximin-analysis}
- Action 2 beats Action 1
- Action 2 beats Action 3
- Action 3 beats Action 1
- Action 3 beats Action 2
:::

<textarea id="maximax-table" class="decision-table">
```{=html}

|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | a       | y       | z      |
| Action 2 | b       | c       | y      |
| Action 3 | b       | d       | z      |
```
</textarea>

::: {#maximax-analysis}
- Action 1 beats Action 2
- Action 1 beats Action 3
- Action 3 beats Action 1
- Action 3 beats Action 2
:::

<textarea id="leximin-table" class="decision-table">
```{=html}

|          | State 1 | State 2 | State 3|
|----------|---------|---------|--------|
| Action 1 | a       | y       | z      |
| Action 2 | b       | c       | y      |
| Action 3 | b       | d       | z      |
```
</textarea>

::: {#leximin-analysis}
- Action 2 beats Action 1
- Action 3 beats Action 1
- Action 3 beats Action 2
:::

<!--more-->
