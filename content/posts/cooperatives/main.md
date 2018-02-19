---
title: The scarcity of cooperatives
published: 2015-04-05
tags: automaton, cooperatives, economics
js: elm-runtime, Automaton
css: cooperatives
graph-of-contents: cooperatives
---

# Introduction
The [predominance of capital-managed firms
(CMF) over worker cooperatives (WC)](#cooperatives-map){#predominate .arg-map} remains an open question in economics.
Early explanations relied on a hypothesized comparative inefficiency of
cooperatives. Subsequent empirical study has shown that
[cooperatives are at least as efficient
as capital-managed firms](#cooperatives-map){#efficient .arg-map} [@doucouliagos95] [@estrin87] [@craig95]
[@levine90].

A profusion of hypotheses has since arisen. [@dow99] offers a good summary
(though the term must be used loosely for a 126-page paper). One that I have not
seen presented is: [capital-managed firms
predominate because capitalists have a greater incentive to expand than
worker-owners in worker cooperatives](#cooperatives-map){#hypothesis .arg-map}. Roughly, for each market segment a
capitalist expands into, their income increases by capital's share of the new
segment's profit. For each market segment a cooperative expands into, the
expanders receive no direct remuneration (supposing that the new market segment
is also a cooperative). Any new profit goes to worker-owners in the new segment.

<!--more-->

# Cellular automaton

We can make this hypothesis more tangible by representing it as a
[cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton).
[[In this
automaton](#cooperatives-map){#automaton-link .arg-map}]{.noted}[^elm], [each cell represents
a market segment requiring a fixed quantity of labor and capital](#cooperatives-map){#segment .arg-map}. Adjacent
cells represent similar market segments.

<div id="automaton" />

## Initial conditions

In the beginning, the market is filled with [empty
market segments which have a random cost](#cooperatives-map){#empty .arg-map} (represented in the automaton by
the opacity of the [red cell interiors]{.empty}) for a firm to
expand into.

## Step

### Occupied market segments

In each step, an existing firm may go bankrupt (or exit the market
segment in some other way) with fixed probability $B = 0.1$. If a firm goes
bankrupt, its market segment becomes empty once again.

If it does not go bankrupt, [the profits generated
during that step are distributed](#cooperatives-map){#accum .arg-map}. For worker cooperatives, all profits
accrue to the worker-owners within the cooperative. For capital-managed
segments, labor's share of income (estimated at 70% [@karabarbounis13]
[@gomme04]) accrues to the segment's workers and capital's share of income
accrues to the capitalists of the segment's firm.

The accumulated income of workers in a capital-managed segment is represented in
the automaton by opacity of the [purple cell
interior]{.capital}. The accumulated income of the capitalists (of a given firm) is
represented by the opacity of the [purple cell
border]{.capital} enclosing all segments owned by the firm. The accumulated income
of worker-owners in a worker cooperative is represented in the automaton by the
opacity of the [green cell interior]{.labor}. Worker
cooperatives sharing an ancestor (e.g. cooperative B and C were both founded by
cooperative A) are enclosed by a single border.

### Empty market segments

In each step, an empty market segment may be occupied by
[a newly formed firm](#cooperatives-map){#firm .arg-map} with chance $N = 0.001$.

Also, in each step, [an empty market segment may
be subject to expansion](#cooperatives-map){#expand .arg-map} from
[adjacent firms](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood).
Each adjacent firm has a 20% chance of attempting expansion (representing market
conditions, firm conditions, &c.). The cost of expansion into the market segment
must be less than the accumulated income of the expander and less than the
projected value of the segment. In the event of multiple firms competing to
expand into a single segment, the firm with the greatest valuation for the
segment succeeds.

Valuations are determined thus:

Capital-managed firm
  :  The total value a capital-management firm can expect to extract from a
     segment takes the form
     $KP + KP(1 - B)(1 - D) + KP(1 - B)^2(1 - D)^2 + \ldots$ where $K$ is
     capital's share of income, $P$ is the per-step profit, $B$ is the
     bankruptcy rate, and $D$ is the firm's
     [discount rate](https://en.wikipedia.org/wiki/Present_value) (ranging
     uniformly from $0$ to $0.2$). Using the
     formula for
     [geometric series](https://en.wikipedia.org/wiki/Geometric_series#Formula),
     we can write this as $\frac{KP}{1 - (1 - B)(1 - D)}$.
Worker cooperative
  :  Similarly, the total value a worker cooperative can expect to extract from
     a segment takes the form  $\frac{AS}{1 - (1 - B)(1 - D)}$. $S = GP -
     LP$, where $G$ is the productivity advantage of worker cooperatives (set
     to 1.1 in the automaton) and $L$ is labor's share of income, represents the
     comparative benefit for worker-owners in a worker cooperative over being
     laborers in a capital-managed firm. [
     $A$ ("altruism") represents the extent to which the expanding worker
     cooperative cares about these potential gains.](#cooperatives-map){#altruism .arg-map}

A quick consequence of this model is that worker cooperatives and
capital-managed firms will value expansions equally (assuming equal discount and
bankruptcy rates) when

$$\begin{align}
\frac{AS}{1 - (1 - B)(1 - D)} &= \frac{KP}{1 - (1 - B)(1 - D)} \\
AS &= KP \\
A(GP - LP) &= KP \\
AP(G - L) &= KP \\
A(G - L) &= K \\
A &= \frac{K}{G - L} \\
A &= \frac{K}{K - 1 + G} \\
\end{align}$$

# Future work

- Allow manual specification of automaton initial state
- Increase fidelity of model
- Permit tweaking of other parameters
- Provide analysis in terms of
[evolutionarily stable strategy](https://en.wikipedia.org/wiki/Evolutionarily_stable_strategy)
- Survey of how cooperatives decide to expand
- Empirical examination of rate of worker cooperative formation and altruism
  (charitable giving, diversity [@putnam07], &c.)
- Empirical examination of rate of worker cooperative formation and labor's
  share of income

[^elm]: Sorry, [Elm](http://elm-lang.org/), Firefox, and this program don't seem
to mix well.

<hr class="references">

<script type="text/javascript">
document.addEventListener("DOMContentLoaded", function() {
    Elm.embed(Elm.Automaton, document.querySelector('#automaton'));
});
</script>
