---
title: An example of the lazy approach to AI safety
published: 2018-07-10
tags: machine ethics, moral uncertainty
css: example-lazy-approach-ai-safety
series: Lazy AI safety
---

Examples often clarify. Let's see an example of the [lazy approach to AI safety](/posts/lazy-ai-safety/) in action.

# The setting

Suppose [The Professor](https://en.wikipedia.org/wiki/The_Professor_(Gilligan%27s_Island)) has performed another bamboo miracle and built an AI agent on the [island](https://en.wikipedia.org/wiki/Gilligan%27s_Island). Sadly, the castaways forgot the agent in their frantic final escape. So it's just our agent, alone on an island in the Pacific.

As a man of taste and refinement, the professor has followed the lazy approach to AI safety. As such, the agent's utility futility is quite simple: The utility of any state of affairs is exactly the moral good of that state of affairs according to whatever turns out to be the [One True Moral Theory (OTMT)]{.noted}[^one-theory]. In symbols, $u(x) = g(x)$ where $u : X \rightarrow \mathbb{R}$ and [$g : X \rightarrow \mathbb{R}$]{.noted}[^good-real] where $X$ is the set of possible states of affairs, $u$ is the utility function, and $g$ evaluates the moral goodness of a state of affairs according to the OTMT.

For simplicity, we'll suppose there are only two possible interventions the agent can make: [Ze](https://en.wiktionary.org/wiki/ze) can harvest coconuts or harvest bamboo. Furthermore, we'll fiat that there are only two possible moral theories in all the world: the coconut imperative and bamboocentrism. According to the coconut imperative, the goodness of a state of affairs is defined as $g_c(b, c) = 0 \cdot b + 3 \cdot c$ where $c$ is the total number of coconuts that have been harvested and $b$ is the total number of bamboo shoots that have been harvested. On the bamboocentric view of things, $g_b(b, c) = 2 \cdot b + 0 \cdot c$. (The fact that we only have moral theories which express goodness in terms of real numbers permits our earlier simplification of assuming that the OTMT takes this shape.)

# Initial behavior

Before the Professor [abandoned his child](https://giphy.com/gifs/will-arnett-UGAwRa9KWjO2Q/fullscreen), he programmed the agent with a uniform prior over all possible ethical theories. That is, the agent thinks there's a 50% chance bamboocentrism is true and a 50% chance the coconut imperative is the OTMT. Thus, in the absence of better information, the agent spends zir days harvesting coconuts (we assume the resources required to harvest a coconut are identical to the resources required to harvest a bamboo stalk). To be fully explicit:

<!--more-->

\begin{align*}
E[\Delta u(h_b)] &= E[u(b + 1, c) - u(b, c)] \\
&= E[u(b + 1, c)] - E[u(b, c)] \\
&= E[g(b + 1, c)] - E[g(b, c)] \\
&= (\frac{1}{2} \cdot g_b(b + 1, c) + \frac{1}{2} \cdot g_c(b + 1, c)) - (\frac{1}{2} \cdot g_b(b, c) + \frac{1}{2} \cdot g_c(b, c)) \\
&= \frac{1}{2} \cdot g_b(b + 1, c) - \frac{1}{2} \cdot g_b(b, c) \\
&= \frac{1}{2} (2 \cdot (b + 1) + 0 \cdot c) - \frac{1}{2} (2 \cdot b + 0 \cdot c) \\
&= 1
\end{align*}

\begin{align*}
E[\Delta u(h_c)] &= E[u(b, c + 1) - u(b, c)] \\
&= E[u(b, c + 1)] - E[u(b, c)] \\
&= E[g(b, c + 1)] - E[g(b, c)] \\
&= (\frac{1}{2} \cdot g_b(b, c + 1) + \frac{1}{2} \cdot g_c(b, c + 1)) - (\frac{1}{2} \cdot g_b(b, c) + \frac{1}{2} \cdot g_c(b, c)) \\
&= \frac{1}{2} \cdot g_c(b, c + 1) - \frac{1}{2} \cdot g_c(b, c) \\
&= \frac{1}{2} (0 \cdot b + 3 \cdot (c + 1)) - \frac{1}{2} (0 \cdot b + 3 \cdot c) \\
&= 1.5
\end{align*}

In words, the expected utility gain from harvesting a bamboo stalk $E[\Delta u(h_b)]$ is $1$ util and thus less than the expected utility gain of $1.5$ from harvesting a coconut $E[\Delta u(h_c)]$.

# Moral investigation

Now, suppose a magic [Magic 8-Ball](https://en.wikipedia.org/wiki/Magic_8-Ball) washes up on the island as narrative devices so obligingly did in '60s television. Somehow this 8-Ball is a moral oracle able to reveal the OTMT with certainty and somehow the agent knows and believes this. In exchange for this knowledge, the 8-Ball asks for nothing less than an *immortal SOUL*---\*cough\* some utils. Should the agent accept this utils for info bargain? The lazy approach to AI safety perspective suggests the next step is to calculate the value of this moral information.

If we go back to the [value of information calculator](/posts/value-information-calculator/) and paste in our scenario:

```
- outcome:
    finding: coconut imperative is correct
    choices:
    - choice: harvest coconut
      results:
      - outcome: {label: coconut imperative, value: 3}
        prob: 1
      - outcome: {label: bamboocentrism, value: 0}
        prob: 0
    - choice: harvest bamboo
      results:
      - outcome: {label: coconut imperative, value: 0}
        prob: 1
      - outcome: {label: bamboocentrism, value: 2}
        prob: 0
  prob: 0.5
- outcome:
    finding: bamboocentrism is correct
    choices:
    - choice: harvest coconut
      results:
      - outcome: {label: coconut imperative, value: 3}
        prob: 0
      - outcome: {label: bamboocentrism, value: 0}
        prob: 1
    - choice: harvest bamboo
      results:
      - outcome: {label: coconut imperative, value: 0}
        prob: 0
      - outcome: {label: bamboocentrism, value: 2}
        prob: 1
  prob: 0.5
```

we find that the expected value of information is $1$. This makes sense. Based on the agent's current beliefs, there's a 50% chance ze'll find out bamboocentrism is true and harvest bamboo accordingly. Ze'd generate $2$ utils in such a scenario. There's also a 50% chance that the 8-Ball will reveal that the coconut imperative is correct and the agent would get $3$ utils there for acting accordingly. So the expected value with perfect information is $2.5$ and the current expected value is $1.5$. $2.5 - 1.5 = 1$ so the agent should be willing to pay up to $1$ util per expected future harvesting decision. For example, if the agent expects the island to be consumed in a fiery volcano imminently (precluding further harvesting), ze shouldn't sacrifice any utils for the info. On the other hand, if the agent will make ten more harvesting decisions, ze should be willing to pay the 8-Ball up to $10$ utils.

# Outro

Obviously, our example is grossly simplified. Among other things, we assume a tiny set of moral theories, a tiny fixed set of actions, and a moral oracle. The hope is that the simplification makes the core, novel elements easier to grasp and that the complexities can be reintroduced later.

[^one-theory]: For the sake of simplicity in our exposition, we'll assume a metaethical view which makes this sensible without justifying that assumption here.
[^good-real]: We're also assuming goodness ought to be mapped to the reals. We'll address this shortly.
