---
title: Some things which are and aren't Pareto optimal
subtitle: Your periodic reminder that Pareto efficiency carries exceedingly little normative weight
published: 2018-04-13
tags: social welfare, metrics, self-flagellation friday
---

# Definition and context

Briefly, a scenario ([canonically, a distribution of resources across individuals]{.noted}[^generalize]) is [Pareto optimal](https://en.wikipedia.org/wiki/Pareto_efficiency) in a set of scenarios if no other scenario is weakly preferred by _all_ individuals (to be more precise, scenario A is a Pareto improvement over scenario B if no individual prefers scenario B to scenario A and at least one individual prefers scenario A). An immediate consequence of this definition is that Pareto improvement is a strict [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set). That is, in opposition to a [total order](https://en.wikipedia.org/wiki/Total_order), not all pairs of scenarios can be ranked; sometimes we can only throw up our hands and say, "I don't know which is better. They both count as Pareto optimal."

[Economists, policy makers and others like the tools of Pareto optimality and improvement]{.noted}[^strawman] because they allow us to make some claims about the societal ranking of outcomes using only individual rankings of outcomes as input. That is, Pareto optimal is a [thin](https://plato.stanford.edu/entries/thick-ethical-concepts/) concept which does not rely on the analyst's moral intuition to make any controversial moral claims or tradeoffs. We expect all non-sadists to agree that a Pareto improvement is a moral improvement while we expect some to disagree with the [Difference Principle](https://en.wikipedia.org/wiki/Justice_as_Fairness#Difference_principle) or with Parfit's [impersonal ethics](https://plato.stanford.edu/entries/identity-ethics/#IdeNorEth).

# Sets of scenarios without Pareto improvements

<table>
<thead>
<tr><th>Scenario</th><th>Person A</th><th>Person B</th></tr>
<tr><th></th><th>no. widgets possessed</th><th>no. widgets possessed</th></tr>
</thead>
<tbody>
<tr><td>1</td><td>0</td><td>100</td></tr>
<tr><td>2</td><td>1</td><td>99</td></tr>
<tr><td>3</td><td>45</td><td>55</td></tr>
<tr><td>4</td><td>50</td><td>50</td></tr>
</tbody>
</table>

::: skippable
- Scenario 1 isn't a Pareto improvement over scenarios 2--4 because person A has fewer goods in scenario 1. (We assume that people prefer more goods to fewer.)
- Scenario 2 isn't a Pareto improvement over scenarios 3 or 4 because person A has fewer goods in scenario 2. It's not a Pareto improvement over scenario 1 because person B has fewer goods in scenario 2.
- Scenario 3 isn't a Pareto improvement over scenario 4 because person A has fewer goods in scenario 3. It's not a Pareto improvement over scenario 1 or 2 because person B has fewer goods in scenario 3.
- Scenario 4 isn't a Pareto improvement over scenarios 1--3 because person B has fewer goods in scenario 4.
:::

<!--more-->

<table>
<thead>
<tr><th>Scenario</th><th colspan="3">Person A</th><th colspan="3">Person B</th></tr>
<tr><th></th><th>Gross income</th><th>Tax</th><th>Subsidy</th><th>Gross income</th><th>Tax</th><th>Subsidy</th></tr>
</thead>
<tbody>
<tr><td>1</td><td>50</td><td>0</td><td>5</td><td>50</td><td>5</td><td>0</td></tr>
<tr><td>2</td><td>50</td><td>0</td><td>0</td><td>50</td><td>0</td><td>0</td></tr>
<tr><td>3</td><td>0</td><td>0</td><td>5</td><td>100</td><td>5</td><td>0</td></tr>
<tr><td>4</td><td>10</td><td>10</td><td>0</td><td>90</td><td>0</td><td>10</td></tr>
</tbody>
</table>

::: skippable
- Scenario 1 isn't a Pareto improvement over scenarios 2--4 because person B has lower net income in scenario 1. (We assume that people prefer more income to less.)
- Scenario 2 isn't a Pareto improvement over scenarios 3 or 4 because person B has lower net income in scenario 2. It's not a Pareto improvement over scenario 1 because person A has lower net income in scenario 2.
- Scenario 3 isn't a Pareto improvement over scenario 4 because person B has lower net income in scenario 3. It's not a Pareto improvement over scenario 1 or 2 because person A has lower net income in scenario 3.
- Scenario 4 isn't a Pareto improvement over scenarios 1--3 because person A has lower net income in scenario 4.
:::

| Scenario | Diabetic         | Thief                    |
|----------|------------------|--------------------------|
|        1 | Consumes insulin | Does nothing             |
|        2 | Foot amputation  | Steals and sells insulin |

::: skippable
- Scenario 1 isn't a Pareto improvement over scenario 2 because the thief has less income in scenario 1.
- Scenario 2 isn't a Pareta improvement over scenario 1 because the diabetic has lost a foot in scenario 2. (We assume that people prefer more feet to fewer.)
:::

| Scenario | Hitler       | World     |
|----------|--------------|-----------|
|        1 | Assassinated | Safe      |
|        2 | Safe         | Holocaust |

::: skippable
- Scenario 1 isn't a Pareto improvement over scenario 2 because Hitler is killed in scenario 1. (We assume people prefer life to death.)
- Scenario 2 isn't a Pareto improvement over scenario 1 because victims of the Holocaust are killed in scenario 2.
:::

# Scenarios with Pareto improvements

Person A and B split a pie in thirds. They each eat one third of the pie and throw the final third in the trash. Assuming everyone prefers more pie to less, giving half the pie to each person is a Pareto improvement.

The [K foundation burns a million quid](https://en.wikipedia.org/wiki/K_Foundation_Burn_a_Million_Quid). Assuming people prefer more money to less and no one cares about performance art, it's a Pareto improvement to _not_ burn the million quid.

Person A has 100 apples and is indifferent between 99 apples and 100 apples. Person B has 100 oranges and is indifferent between 99 oranges and 100 oranges. Assuming both parties prefer 1 of any given fruit to 0, it would be a Pareto improvement for person A to trade their 100th apple in exchange for person B's 100th orange.

# Belaboring the obvious

The Pareto approach cannot render judgment as to weather the Holocaust is better than not-the-Holocaust. On the other hand, it condemns foregone trade. This is not an approach that should take pride of place in evaluating policies.

[^generalize]: In what follows, we'll allow the obvious extension to scenarios that vary on dimensions other than just resource distributions---that is, we'll apply the same logic of Pareto improvements and abjuring interpersonal tradeoffs to scenarios which vary in institutions, culture, social relationships, etc.

[^strawman]: You may object that I'm erecting a [straw man](https://en.wikipedia.org/wiki/Straw_man). There is some truth to this objection; I hope no competent analyst would, after thoughtful deliberation, endorse Pareto efficiency as an ultimate or sufficient ethical framework. However, I think when making decisions reflexively, we can forget the severe limitations of the Pareto approach---I know I sometimes do. Thus, as the subtitle suggests, this reminder which seeks to make the deficiencies more visceral and [available](https://en.wikipedia.org/wiki/Availability_heuristic).
