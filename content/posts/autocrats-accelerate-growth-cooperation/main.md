---
title: Autocrats <em>can</em> accelerate growth through cooperation
subtitle: Disagreeing with <i>Institutions as a fundamental cause of long-run growth</i>
published: 2018-06-01
tags: political economy, autocracy
series: Why Nations Fail
---

# Intro

In [@acemoglu2005] and [@acemoglu2013] (and surely elsewhere), Acemoglu and Robinson contend that inclusive societies have inherent economic advantages over stratified societies. If true, this is quite important; it suggests that, in the long run, we should [expect inclusive societies to "win" out]{.noted}[^inclusive-win]. The moral arc of the universe bends toward justice and all that.

# Scenario

But is it true? As far as I can tell, the best articulation of this claim is in section 6 of [@acemoglu2005]. It lays out several claims in support of the argument that oligarchic societies have a structural disadvantage when it comes to economic growth. In this post, we'll focus on the first:

<blockquote>Imagine a situation in which an individual or a group holds unconstrained political power. Also suppose that productive investments can be undertaken by a group of citizens or producers that are distinct from the "political elites", i.e., the current power holders. The producers will only undertake the productive investments if they expect to receive the benefits from their investments. Therefore, a set of economic institutions protecting their property rights are necessary for investment. Can the society opt for a set of economic institutions ensuring such secure property rights? The answer is often no (even assuming that "society" wants to do so).

The problem is that the political elites---those in control of political power---cannot commit to respect the property rights of the producers once the investment are undertaken. Naturally, ex ante, before investments are undertaken, they would like to promise secure property rights. But the fact that the monopoly of political power in their hands implies that they cannot commit to not hold-up producers once the investments are sunk.

[...] The consequence is clear: without such protection, productive investments are not undertaken, and opportunities for economic growth go unexploited.
</blockquote>

# Counterclaim

Contrary to the claim here, I think a rational and informed autocrat could credibly support economic institutions like property rights. If the autocrat and the producers interact over a non-trivial period of time, producers can condition their behavior on the autocrat's. That is, the producers can decide that they'll only invest if the autocrat preserves economic freedoms. If the autocrat expropriates and otherwise abuses the producers, they'll do the bare minimum (shirk). Knowing this, the autocrat realizes that they can't simply choose to loot while the producers continue to invest. The autocrat's choice is between looting a minimal economy and a more restrained taxation of a growing economy. Under the right circumstances, a rational autocrat would do best to choose taxing a growing economy rather than looting a minimal economy.

# Model

## Single-shot

Let's model this situation more explicitly to make our claims precise. We can describe it as a two player [game](https://en.wikipedia.org/wiki/Game_theory). Player one is the $\text{Autocrat}$ and player two is the [$\mathtt{Producers}$]{.noted}[^producers]. During the game, the $\text{Autocrat}$ must decide to either $\text{Tax}$ at a sustainable rate or $\text{Loot}$ by taxing at an onerous rate, expropriating and otherwise taking a larger share of the economic output. We represent these actions as fractions of economic output taken by the $\text{Autocrat}$ with $1 \geq l > k \geq 0$. The $\mathtt{Producers}$ must decide to $\mathtt{Shirk}$---just get by with a minimum amount of work at a subsistence standard of living---or $\mathtt{Invest}$---devote more effort to production in the hopes of future improvements to productivity. The extra cost that producers pay when $\mathtt{Investing}$ is represented by $c > 0$. In either case, $\mathtt{Producers}$ receive whatever fraction of the economic output the dictator doesn't take---$1 - k$ or $1 - l$. This is all described more succinctly in the table below:

<figure>
<figcaption>Single-shot game of economic production in an autocratic regime</figcaption>
| $\text{Autocrat} \backslash \mathtt{Producers}$ | $\mathtt{Invest}$                   | $\mathtt{Shirk}$              |
|:------------------------------------------------|:------------------------------------|:------------------------------|
| $\text{Tax}$                                    | $k \backslash \mathtt{(1 - k) - c}$ | $k \backslash \mathtt{1 - k}$ |
| $\text{Loot}$                                   | $l \backslash \mathtt{(1 - l) - c}$ | $l \backslash \mathtt{1 - l}$ |
</figure>

In this game, Acemoglu's and Robinson's claim is correct. $\text{Tax}\backslash\mathtt{Invest}$ is not an equilibrium. The $\text{Autocrat}$ can always improve their outcome by switching from a strategy of $\text{Tax}$ to $\text{Loot}$. Similarly, without any opportunity to recoup the increased cost of $\mathtt{Investing}$, $\mathtt{Producers}$ will always prefer $\mathtt{Shirking}$.

<!--more-->

## Iterated

### Setup

But this isn't a good model of the situation. $\text{Autocrats}$ and $\mathtt{Producers}$ interact repeatedly over a sustained period of time. We have a [repeated game](https://en.wikipedia.org/wiki/Repeated_game). Because no one knows when an autocrats reign will end, we avoid a [finitely repeated game](https://en.wikipedia.org/wiki/Repeated_game#Finitely_repeated_games) and model this is an [infinitely repeated game](https://en.wikipedia.org/wiki/Repeated_game#Infinitely_repeated_games) with a discount rate $\delta$ or, less pessimistically, as a game with some fixed probability of continuing after each stage $\delta$.

Now that the game has a sequence of stages, we can meaningfully incorporate the impact of $\mathtt{Investing}$. The basic intuition is that there is some growth rate [$\frac{1}{\delta} > r \geq 1$]{.noted}[^growth-rate] and each instance of $\mathtt{Investment}$ compounds it. More formally, the productivity produced by actions up through time $t$ is $p(\mathbf{a^t}) = r^{\sum_{t=0}^t \mathtt{I}(a_{\mathtt{R}}^t)}$ where $I$ is an indicator function defined as $$
\mathtt{I}(a_{\mathtt{R}}^t) = \begin{cases}
  1 & a_{\mathtt{R}}^t = \mathtt{Invest} \\
  0 & a_{\mathtt{R}}^t = \mathtt{Shirk} \\
\end{cases}
$$.

We just use this productivity multiplicatively with our previous terms so the new stage game looks like:

<figure>
<figcaption>Stage of repeated game of economic production in an autocratic regime. Incorporates conditional productivity growth.</figcaption>
| $\text{Autocrat} \backslash \mathtt{Producers}$ | $\mathtt{Invest}$                                   | $\mathtt{Shirk}$                                |
|:------------------------------------------------|:----------------------------------------------------|:------------------------------------------------|
| $\text{Tax}$                                    | $p \cdot k \backslash \mathtt{p \cdot (1 - k) - c}$ | $p \cdot k \backslash \mathtt{p \cdot (1 - k)}$ |
| $\text{Loot}$                                   | $p \cdot l \backslash \mathtt{p \cdot (1 - l) - c}$ | $p \cdot l \backslash \mathtt{p \cdot (1 - l)}$ |
</figure>

### Analysis

The utility of player $i$ given all actions of all players across time ($\mathbf{a}$) is $U_i(\mathbf{a}) = \sum_{t=0}^\infty \delta^t \cdot u_i(a_i^t, a_{-i}^t, \mathbf{a^{t-1}})$.

We'll start by examining the utility obtained by each player when they pursue a constant strategy (e.g. *always* invest, *always* loot).

#### $\text{Tax}\backslash\mathtt{Invest}$

In the first round, $\mathtt{Producers}$ invest but have not yet reaped the benefit. So they obtain only $(1 - k) - c$ (what's left after the $\text{Autocrat}$'s taxes minus the cost of $\mathtt{Investing}$) while the $\text{Autocrat}$ simply receives their tax share of output $k$. In all subsequent rounds, output is scaled by the competing factors of the discount rate $\delta$ and the growth rate $r$ while cost is scaled only by the discount rate $\delta$.

<figure>
<figcaption>Discounted utility obtained by the $\text{Autocrat}$ and $\mathtt{Producers}$ given they play $\text{Tax}$ and $\mathtt{Invest}$ against each other.</figcaption>
$$
\begin{align*}
U_{\text{A}}(\mathbf{a}) &= k + \sum_{t=1}^\infty \delta^t \cdot r^t \cdot k \\
&= k + \frac{\delta \cdot r \cdot k}{1 - \delta \cdot r} \\
U_{\mathtt{P}}(\mathbf{a}) &= (1 - k) - c + \sum_{t=1}^\infty \delta^t \cdot (r ^t \cdot (1 - k) - c) \\
&= (1 - k) - c + \frac{\delta \cdot r \cdot (1 - k)}{1 - \delta \cdot r} - \frac{\delta \cdot c}{1 - \delta} \\
\end{align*}
$$
</figure>

#### $\text{Loot}\backslash\mathtt{Invest}$

The setup is identical here except that the lower $\text{Tax}$ rate $k$ has been replaced by the higher $\text{Looting}$ rate $l$.

<figure>
<figcaption>Discounted utility obtained by the $\text{Autocrat}$ and $\mathtt{Producers}$ given they play $\text{Loot}$ and $\mathtt{Invest}$ against each other.</figcaption>
$$
\begin{align*}
U_{\text{A}}(\mathbf{a}) &= l + \sum_{t=1}^\infty \delta^t \cdot r^t \cdot l \\
&= l + \frac{\delta \cdot r \cdot l}{1 - \delta \cdot r} \\
U_{\mathtt{P}}(\mathbf{a}) &= (1 - l) - c + \sum_{t=1}^\infty \delta^t \cdot (r ^t \cdot (1 - l) - c) \\
&= (1 - l) - c + \frac{\delta \cdot r \cdot (1 - l)}{1 - \delta \cdot r} - \frac{\delta \cdot c}{1 - \delta} \\
\end{align*}
$$
</figcaption>

#### $\text{Tax}\backslash\mathtt{Shirk}$

In the first round, $\mathtt{Producers}$ don't invest so they simply receive $1 - k$. Similarly, the $\text{Autocrat}$ receives only $k$ in the first round. In all subsequent rounds, players' rewards are scaled by the discount factor $\delta$.

<figure>
<figcaption>Discounted utility obtained by the $\text{Autocrat}$ and $\mathtt{Producers}$ given they play $\text{Tax}$ and $\mathtt{Shirk}$ against each other.</figcaption>
$$
\begin{align*}
U_{\text{A}}(\mathbf{a}) &= k + \sum_{t=1}^\infty \delta^t \cdot k \\
&= k + \frac{\delta \cdot k}{1 - \delta} \\
U_{\mathtt{P}}(\mathbf{a}) &= (1 - k) + \sum_{t=1}^\infty \delta^t \cdot (o \cdot (1 - k)) \\
&= (1 - k) + \frac{\delta \cdot (1 - k)}{1 - \delta} \\
\end{align*}
$$
</figcaption>

#### $\text{Loot}\backslash\mathtt{Shirk}$

The setup is identical here except that the lower $\text{Tax}$ rate $k$ has been replaced by the higher $\text{Looting}$ rate $l$.

<figure>
<figcaption>Discounted utility obtained by the $\text{Autocrat}$ and $\mathtt{Producers}$ given they play $\text{Loot}$ and $\mathtt{Shirk}$ against each other.</figcaption>
$$
\begin{align*}
U_{\text{A}}(\mathbf{a}) &= l + \sum_{t=1}^\infty \delta^t \cdot l \\
&= l + \frac{\delta \cdot l}{1 - \delta} \\
U_{\mathtt{P}}(\mathbf{a}) &= (1 - l) + \sum_{t=1}^\infty \delta^t (o \cdot (1 - l)) \\
&= (1 - l) + \frac{\delta \cdot (1 - l)}{1 - \delta} \\
\end{align*}
$$
</figure>

#### Equilibrium with constant strategies

Examination makes it clear that, with only these constant strategies available, $\text{Looting}$ is still the best strategy for the $\text{Autocrat}$. How should the $\mathtt{Producers}$ respond? That depends on the parameters. If we copy our utility expressions from above for $\mathtt{Investing}$ and $\mathtt{Shirking}$ and set them equal, we find the critical growth rate $r$:

<figure>
<figcaption>Parameter values at which $\text{Looted}$ $\mathtt{Producers}$ are indifferent between $\mathtt{Investing}$ and $\mathtt{Shirking}$.</figcaption>
$$
U_{\mathtt{I}}(\mathbf{a}) = (1 - l) - c + \frac{\delta \cdot r \cdot (1 - l)}{1 - \delta \cdot r} - \frac{\delta \cdot c}{1 - \delta} \\
U_{\mathtt{S}}(\mathbf{a}) = (1 - l) + \frac{\delta \cdot (1 - l)}{1 - \delta} \\
(1 - l) - c + \frac{\delta \cdot r \cdot (1 - l)}{1 - \delta \cdot r} - \frac{\delta \cdot c}{1 - \delta} = (1 - l) + \frac{\delta \cdot (1 - l)}{1 - \delta} \\
r = 1 + \frac{c \cdot (1 - \delta)}{c \cdot \delta + \delta \cdot (1 - l)}
$$

For $r$ less than this, $\mathtt{Shirking}$ is preferred. The intuition here is simply that it makes no sense to incur the costs of investing if the payoff is too low (or even 0).

So we find ourselves in the same equilibrium again than. When only constant strategies are available, $\text{Loot}$ and $\mathtt{Shirk}$ is the equilibrium.

#### Tit for tat

In repeated games, we can encourage cooperation by punishing defection. What happens if we permit $\mathtt{Producers}$ to use a [tit for tat strategy](https://en.wikipedia.org/wiki/Trigger_strategy)? First, we'll note that if the $\text{Autocrat}$ $\text{Taxes}$, the $\mathtt{Producers}$ never need to punish and so it plays out exactly like $\text{Tax}\backslash\mathtt{Invest}$. On the other hand, if the autocrat follows a constant $\text{Loot}$ strategy, the $\mathtt{Producers}$ $\mathtt{Invest}$ in the first round and never again.

<figure>
<figcaption>Discounted utility obtained by the $\text{Autocrat}$ and $\mathtt{Producers}$ given they play $\text{Loot}$ and $\mathtt{Tit-for-tat}$ against each other.</figcaption>
$$
\begin{align*}
U_{\text{A}}(\mathbf{a}) &= l + \sum_{t=1}^\infty \delta^t \cdot r \cdot l \\
&= l + \frac{\delta \cdot r \cdot l}{1 - \delta} \\
U_{\mathtt{P}}(\mathbf{a}) &= (1 - l) - c + \sum_{t=1}^\infty \delta^t (o \cdot r \cdot (1 - l) - c) \\
&= (1 - l) - c + \frac{\delta \cdot r \cdot (1 - l)}{1 - \delta} \\
\end{align*}
$$
</figcaption>

It turns out that $\text{Tax}, \mathtt{Tit-for-tat}$ is an equilibrium (given the strategy space and the appropriate parameters).

To see this, we first confirm that the $\mathtt{Producer}$ can do no better. Here, we appeal to our earlier claim that the payoffs for this game are the same as for $\text{Tax}, \mathtt{Invest}$ so no improvement is possible there. Comparing $\mathtt{Tit-for-tat}$ to $\mathtt{Shirking}$ while $\text{Taxed}$ is also structurally identical to our earlier comparison of $\mathtt{Investing}$ to $\mathtt{Shirking}$ while $\mathtt{Looted}$. We just swap out the higher tax rate $l$ in favor of a $k$. The critical growth rate then is $r = 1 + \frac{c \cdot (1 - \delta)}{c \cdot \delta + \delta \cdot (1 - k)}$. For $r$ less than this, $\mathtt{Producers}$ prefer $\mathtt{Shirking}$ and otherwise $\mathtt{Investing}$.

Now we have only to confirm that the $\text{Autocrat}$ can't improve their lot by $\text{Looting}$. Again, we'll copy over the two utility expressions and set them equal to find the critical growth rate $r$.

<figure>
<figcaption>Parameter values at which a $\mathtt{Tit-for-tatted}$ $\text{Autocrat}$ is indifferent between $\text{Taxing}$ and $\text{Looting}$.</figcaption>
$$
U_{\text{T}}(\mathbf{a}) = k + \frac{\delta \cdot r \cdot k}{1 - \delta \cdot r} \\
U_{\text{L}}(\mathbf{a}) = l + \frac{\delta \cdot r \cdot l}{1 - \delta} \\
l + \frac{\delta \cdot r \cdot l}{1 - \delta} = k + \frac{\delta \cdot r \cdot k}{1 - \delta \cdot r} \\
r = \frac{\sqrt{\delta^2 \cdot l + (4 \cdot \delta - 4) \cdot (k - l)}}{2 \cdot \delta \cdot \sqrt{l}} + \frac{1}{2}
$$
</figure>

If $r$ is greater than this, the $\text{Autocrat}$ will prefer $\text{Taxing}$. That means when $r$ satisfies both of these constraints, $\text{Tax}, \mathtt{Tit-for-tat}$ is an equilibrium.

#### Concrete

Let's check our math and make this a bit more concrete by plugging in some numbers. If we pick $k = 0.2$, $l = 0.8$, $c = 0.8$ and $\delta = 0.95$, we find that our two constraints on $r$ work out to  $r > \frac{39}{38} \approx 1.02632$ and $r \gtrapprox 1.03995$. We'll pick $r = 1.04$. Now plugging in all our parameters, we can calculate all the payoffs. We end up with a game like this:

| $\text{Autocrat} \backslash \mathtt{Producers}$ | $\mathtt{Invest}$        | $\mathtt{Shirk}$     | $\mathtt{Tit-for-tat}$   |
|:------------------------------------------------|:-------------------------|:---------------------|:-------------------------|
| $\text{Tax}$                                    | $16.66 \backslash 50.66$ | $4 \backslash 2.32$  | $16.66 \backslash 50.66$ |
| $\text{Loot}$                                   | $66.66 \backslash 0.66$  | $16 \backslash 0.58$ | $16.61 \backslash 3.352$ |

Happily, this supports our earlier claim that, with the right choice of parameters and strategy space, cooperation is an equilibrium.

# Conclusion

Contra [@acemoglu2005], autocrats can credibly cooperate in support of economic institutions which promote growth. The possibility for cooperation arises when we move from a single-shot game to a repeated game (as is often the case). This is bad news because it means we have one fewer reason to suppose that economics supports inclusivity in the long run.

<hr class="references">

[^inclusive-win]: The victory of inclusive societies is maintained, they argue, by a virtuous circle in which inclusive institutions are robust to perturbations.
[^producers]: We will grant ourselves the unrealistic convenience of modeling the disparate group of producers as a single coordinated actor.
[^growth-rate]: We demand $\delta \cdot r < 1$ for reasons of both mathematical tractability and economic plausibility.
