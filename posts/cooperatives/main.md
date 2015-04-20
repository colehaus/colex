---
title: The scarcity of cooperatives
published: 2014-11-18
tags: automaton, cooperatives, economics
js: /js/elm-runtime.js, /js/Automaton.js, /js/cooperatives.js
css: cooperatives
---

<div id="graph-of-contents"><a href="#arg-map">Contents</a></div>

<div class="abstract">
Why do capital-managed firms predominate over worker cooperatives? Perhaps it's
because worker cooperatives have less incentive to expand.
</div>

# Introduction

The <a href="#arg-map" id="predominate">predominance of capital-managed firms
(CMF) over worker cooperatives (WC)</a> remains an open question in economics.
Early explanations relied on a hypothesized comparative inefficiency of
cooperatives. Subsequent empirical study has shown that
<a href="arg-map" id="efficient">cooperatives are at least as efficient
as capital-managed firms</a> [@doucouliagos95] [@estrin87] [@craig95]
[@levine90].

A profusion of hypotheses has since arisen. [@dow99] offers a good summary
(though the term must be used loosely for a 126 page paper). One that I have not
seen presented is: <a href="#arg-map" id="hypothesis">capital-managed firms
predominate because capitalists have a greater incentive to expand than
worker-owners in worker cooperatives</a>. Roughly, for each market segment a
capitalist expands to, their income increases by capital's share of the new
segment's profit. For each market segment a cooperative expands to, the
expanders receive no direct remuneration (supposing that the new market segment
is also a cooperative). Any new profit goes to worker-owners in the new segment.

<!--more-->

# Cellular automaton

We can make this hypothesis more tangible by representing it as a
[cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton).
<span class="noted"><a href="#arg-map" id="automaton-link">In this
automaton</a></span>[^elm], <a href="#arg-map" id="segment">each cell represents
a market segment requiring a fixed quantity of labor and capital</a>. Adjacent
cells represent similar market segments.

<div id="automaton" />

## Initial conditions

In the beginning, the market is filled with <a href="#arg-map" id="empty">empty
market segments which have a random cost</a> (represented in the automaton by
the opacity of the <span class="empty">red cell interiors</span>) for a firm to
expand into.

## Step

### Occupied market segments

In each step, an existing firm may go bankrupt (or exit the market
segment in some other way) with fixed probability $B = 0.1$. If a firm goes
bankrupt, its market segment becomes empty once again.

If it does not go bankrupt, <a href="#arg-map" id="accum">the profits generated
during that step are distributed</a>. For worker cooperatives, all profits
accrue to the worker-owners within the cooperative. For capital-managed
segments, labor's share of income (estimated at 70% (which is on the high end)
[@karabarbounis13] [@gomme04]) accrues to the segment's workers and capital's
share of income accrues to the capitalists of the segment's firm.

The accumulated profit of workers in a capital-managed segment is represented in
the automaton by opacity of a <span class="capital">purple cell interior</span>.
The accumulated profit of the capitalists for capital-managed segments is
represented by the opacity of a <span class="capital">purple cell border</span>
enclosing all segments owned by the firm. The accumulated profit of
worker-owners in a worker cooperative is represented in the automaton by the
opacity of a <span class="labor">green cell interior</span>. Worker cooperatives
sharing an ancestor (e.g. cooperative B and C were both founded by cooperative
A) will be enclosed by a single border.

### Empty market segments

In each step, an empty market segment may be occupied by
<a href="#arg-map" id="firm">a newly formed firm</a> with chance $N = 0.001$.

Also, in each step, <a href="#arg-map" id="expand">an empty market segment may
be subject to expansion</a> from
[adjacent firms](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood).
Each adjacent firm has a 20% chance of attempting expansion (representing market
conditions, firm conditions, &c.). The cost of expansion into the market segment
must be less than the accumulated profits of the expanding firm and less than
the projected value of the segment. In the event of multiple firms competing
to expand into a single segment, the firm with the greatest valuation for the
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
     laborers in a capital-managed firm. <a href="#arg-map" id="altruism">
     $A$ ("altruism") represents the extent to which the expanding worker
     cooperative cares about these potential gains.</a>

A quick consequence of this model is that worker cooperatives and
capital-managed firms will value expansions equally (assuming equal discount
rates) when

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
- Provide analysis in terms of
[evolutionarily stable strategy](https://en.wikipedia.org/wiki/Evolutionarily_stable_strategy)
- Survey of how cooperatives decide to expand
- Empirical examination of rate of worker cooperative formation and altruism
  (diversity [@putnam07], charitable giving, &c.)
- Empirical examination of rate of worker cooperative formation and labor's
  share of income

[^elm]: Sorry, [Elm](http://elm-lang.org/), Firefox, and this program don't seem
to mix well.

<hr class="references">

<script type="text/javascript">
document.addEventListener("DOMContentLoaded", function() {
    Elm.embed(Elm.Automaton, $('#automaton').get(0));
});
</script>
