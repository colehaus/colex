---
title: Build your own von Neumann–Morgenstern utility function
published: 2018-05-13
tags: decision theory, interactive
css: construct-vnm-utility-function
js: construct-vnm-utility-function
---

# von Neumann--Morgenstern utility theorem

The [von Neumann--Morgenstern utility theorem](https://en.wikipedia.org/wiki/Von_Neumann%E2%80%93Morgenstern_utility_theorem) says that, "under certain axioms of rational behavior, a decision-maker faced with risky (probabilistic) outcomes of different choices will behave as if he or she is maximizing the expected value of some function defined over the potential outcomes at some specified point in the future". But the somewhat sloppy way I like to think of it is this: If a person has merely [ordinal preferences](https://en.wikipedia.org/wiki/Ordinal_utility) (e.g. I prefer an apple to a banana but can't or won't quantify the magnitude of that preference. The preceding information alone isn't enough to conclude how I'd feel about one apple vs. two bananas.) and reasons well under uncertainty, we can transform those ordinal preferences into a [cardinal utility function](https://en.wikipedia.org/wiki/Cardinal_utility) (e.g. I like apples exactly twice as much as bananas and would be indifferent between an apple and two bananas (ignoring diminishing marginal utility for the same of exposition).).

This transformation is often useful because a cardinal utility function is much richer and more informative than an ordinal utility function. The extra information is useful, for example, in sidestepping [Arrow's impossibility theorem](https://en.wikipedia.org/wiki/Arrow%27s_impossibility_theorem) (which says that it's impossible to have a good voting system if you only ask people for their ordering of candidates).

# Interactive VNM

The standard descriptions of the mechanism of the VNM utility theorem may be a little opaque. But because the theorem is [constructive](https://en.wikipedia.org/wiki/Constructive_proof), we can actually give people a feel for it by putting them 'inside' the mechanism and showing them the result. That's what we attempt here.

In the first text area, enter a list of goods (each on a separate line) for which you'd like to generate a utility function. It starts with a few sample goods, but you're free to add, remove or otherwise alter these.

Once you've decided upon the goods you're interested in, you can proceed to the next step. Here, you'll be presented with a series of lotteries. In each lottery, you have to decide whether you prefer $x$ lottery tickets for one good over $y$ lottery tickets for the other good, or if you're indifferent. If your lottery ticket is drawn, you win whatever good is on the ticket. You can register your answer as to which set of tickets you prefer by clicking on one of the three blue boxes.

For example, if you mildly prefer bananas to carrots, you'd click on the banana box when presented with one lottery ticket for each. A $\frac{1}{n}$ chance of a banana is better than a $\frac{1}{n}$ chance of a carrot, by your lights ($n \geq 2$). On the other hand (because your preference was only mild), you'd click on the carrot box if offered 100 carrot tickets vs. 1 banana ticket. A $\frac{100}{n}$ chance of a carrot is better than a $\frac{1}{n}$ chance of a banana ($n \geq 101$).

After you've repeated this process enough, we can deduce what your favorite good of all the listed goods is. With this as a [numéraire
](https://en.wikipedia.org/wiki/Num%C3%A9raire), we can start to visualize your utility function and do so with a chart that appears at the bottom. But, of course, we still have uncertainty about the relative value of these goods. Based on the questions you answer, we know upper and lower bounds for your value (a carrot is better than $\frac{1}{100}$ banana but worse than $\frac{1}{1}$ banana). Over time, by answering more questions, we can refine these intervals until they're arbitrarily small.

Try it out!

---

<form>
<p>Which things would you like to make a utility function out of?</p>

<!--more-->

<textarea id="goods">
```{=html}
Apple
Banana
Carrot
```
</textarea>
<p class="input">Which do you prefer?</p>
<div class="input scenario">
Receive <span id="first-good" class="lottery"><span class="odds">1.00e+0</span> <span class="good">Banana</span> lottery ticket(s)</span><br/><br/>or <span id="second-good" class="lottery"><span class="odds">1.00e+0</span> <span class="good">Carrot</span> lottery ticket(s)</span><br/><br/>
<span id="indifferent" class="lottery">Indifferent</span>
</div>
</form>
<output>
<figure id="function-visualization">
<figcaption>Estimated value of goods relative to favorite good</figcaption>
<div id="function-chart">
</div>
</figure>
</output>

## Aside

I can also imagine the basic setup of VNM as useful for preference elicitation. If you ask respondents in a survey to directly assign cardinal values to various outcomes, I suspect they will have little intuition for the task and generate poor estimates. Presenting them with a series of lotteries is at least a different task and it may turn out to be an easier or more accurate one.

