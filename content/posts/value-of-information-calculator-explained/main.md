---
title: Value of information calculator
published: 2018-06-12
edited: 2019-06-24
tags: value of information, decision theory, interactive
css: value-of-information-calculator
js: value-of-information-calculator
include-toc: true
---

# Motivating scenario

Suppose you wanted to do the absolute *most* good you could. No satisficers here. You wouldn't just luck into the right activity. It would probably require careful thinking and investigating many big, difficult problems. How do you deal with moral uncertainty? [@macaskill2014] Which moral theory should you grant the most credence? What are all the possible do-gooding interventions? What are their unintended consequences?

If you insisted upon answering all these questions before acting, you'd almost surely die before you finished. That's probably not the way to maximize impact. [You should probably act in the world at some point---even filled with doubt and uncertainty---rather than philosophize until Death Himself comes for you.]{.noted}[^philosophy] But when should you stop investigating? One possibility is to just pick a semi-arbitrary date on the calendar---to "timebox".

Can you do better than this? Can we come up with a more principled way to transition from investigation to action? I contend that the answer is "Yes" and that the tool is value of information calculations.

# Expected value of information calculation

## Simple scenario

We'll now look at a much simpler domain for expository purposes. Suppose a friend came to you and offered you a dollar if you called their coin flip correctly. As long as they didn't charge you, it would make sense to agree as you'd expect to win 50 cents on average. Even better would be if you could swap out their coin with your own trusty [two-headed coin](http://tvtropes.org/pmwiki/pmwiki.php/Main/TwoHeadedCoin). Then, you could be certain that you'd make the right call and you'd get the dollar every time. The extra information you get by knowing the outcome has value.

## Expected value of perfect information

Slightly less obvious is that you can believe information is valuable to you without being certain exactly what that information is. Suppose you were unable to swap out the flipper's coin, but a trustworthy friend came to you and whispered, "I know that the flipper uses a two-sided coin. How much will you pay me to tell you whether it's double heads or double tails?". After some thinking, I hope you'll agree that you'd gain by paying up to 50 cents for this information. Without the information you expect to earn 50 cents from the flipper's bargain. With the information, you expect to earn a dollar. If your friend tells you that it's a two-headed coin, you can simply bet on heads. If they tell you it's a two-tailed coin, bet on tails. Either way, you're guaranteed the dollar. As long as you can react accordingly via your bet, you should be willing to pay for this unknown information. Paying, for example, 20 cents would still leave you ahead because your net gain from the info payment and the bet itself be 80 cents instead of the original expected value of 50 cents.

<!--more-->

## Expected value of imperfect information

"But what if my susurrous friend lies or is mistaken?", you rightly object. Even if your friend's whispers don't leave you 100% certain about what kind of coin the flipper has, we can still apply the same logic from before. The arithmetic is just a bit more complicated. Ultimately, the conclusion that reducing our uncertainty can have value---even without knowing in which direction the uncertainty will be reduced---remains intact.

## Other explanations

If you'd like an alternative exposition with more details and some accompanying graphics, I think these are both pretty good: [The Value of Information](http://www.public.asu.edu/~kirkwood/DAStuff/decisiontrees/DecisionTreePrimer-3.pdf), [Value of Information in Decision Trees](http://treeplan.com/chapters/value-of-information-in-decision-trees.pdf).

# Calculator

But you might not need to understand the details of the calculation laid out in those links because I provide a handy value of information calculator for you here.

The input required by the calculator is a tree in [YAML format](https://en.wikipedia.org/wiki/YAML). The top level of the tree describes the different pieces of information you might find (e.g. the coin is double heads) after investigating and how likely you think each piece of information is. Each of these info nodes has children corresponding to actions you might take (e.g. bet heads). The final level of the tree (the children of the action nodes) describe the outcomes that actually occur and their probability of occurrence given the information received.

So the default tree entered below describes the coin flip scenario described above when you have no clue as to which two-sided coin is in use and you believe your susurrous friend is absolutely reliable.

In addition to showing the expected value of information, the calculator shows the corresponding simplified scenario in which you have no way of gaining information about outcomes at the bottom. For example, it shows you the original coin flip scenario before your susurrous friend comes along. This is offered simply as a point of comparison.

## Input

```{=html}
<form id="voi">
<textarea class="voi-tree" id="voi-text">
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
</textarea>
</form>
```

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

[^philosophy]: Of course, the scenario is complicated by the possibility of passing on the progress made and hoping someone else eventually puts the knowledge into action. Since this is just supposed to be a motivating example, we'll ignore that possibility.
