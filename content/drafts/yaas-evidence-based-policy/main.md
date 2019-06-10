---
title: YAAS Evidence-based Policy
subtitle: "Yet Another Amateur Summary: Cartwright and Hardie's <i>Evidence-based Policy: A practical guide to doing it better</i>"
date: 2019-05-25
tags: yaas, applied epistemology
series: Evidence-based Policy
---

# Vignette

The Tamil Nadu Integrated Nutrition Program (TINP) was a program to reduce child malnutrition among the rural poor of India's Tamil Nadu State. Core elements of the program were: nutrition counseling for pregnant mothers and supplementary food for the most deprived young children. TINP succeeded in reducing malnutrition by [TODO: World Bank Independent Evaluation Group, 1995]. Seeing this success, policymakers [TODO] in Bangladesh launched the Bangladesh Integrated Nutrition Program (BINP) modeled on TINP. Unfortunately, six years later, childhood malnutrition in Bangladesh continued unabated [TODO: Save The Children 2003, World Bank 1995].

Why did BINP fail where TINP succeeded? One of the main problems was that, while mothers did indeed learn about childhood nutrition, mothers often weren't the primary decision makers [TODO: hyphen] in matters of nutrition. In rural Bangladesh, men typically do the shopping. And if a mother lived with her mother-in-law, that mother-in-law was the final authority on issues within the women's domain. Because the decision-makers hadn't received BINP's nutrition counseling, they often used supplemental food from BINP as a substitute and reallocated other food away from mother and child.

# Effectiveness

## The problem: Generalization

Even in the absence of that holy grail---the RCT---there's considerable confidence that TINP worked. But BINP didn't. <i>Evidence-Based Policy</i> takes problems like this as it's central concern. How do we move from "It worked there."---[efficacy](#key-terms)---to "It will work here."---[effectiveness](#key-terms)?

## The standard solution: External validity

The standard way of thinking about this problem is [external validity](TODO). Briefly, a study is internally valid when it provides strong reasons to believe its conclusions. A study has external validity when it can be generalized to other contexts---different times, places and populations. 

But <abbr title="Evidence-based Policy">EBP</abbr> disdains external validity. Claims of external validity usually take a shape like "Study A gives us reason to believe that intervention B---which worked on population C at time and place D---will also work on similar (to population C) population E at similar (to time and place D) time and place F." But the word "similar" is doing all the work here. What does it mean? 

"Similar" can't mean "identical"---then all studies would be pointless because we would never have external validity and could never generalize. But "similar" also shouldn't be construed too permissively. If you insist that the population of Manhattanites [TODO] and the population of rural Tibetans are "similar" because they both consist of humans living in communities with hierarchies of esteem under a system of hegemonic capitalism on planet Earth, you'll be find yourself perpetually surprised when your interventions fail to replicate.

Furthermore, similarity means radically different things in different contexts. If the original study is about reducing Alzheimer's for high risk populations, similarity means biomedical similarity and certain elderly people in rural Tibet may in fact be more similar to certain elderly people in Manhattan than either subpopulation is to their neighbors. On the other hand, if the study is about the welfare effects of exposure to pervasive advertising, rural Tibet and Manhattan count as pretty dissimilar.

So "similar" has to mean similar in [the right ways and to the right degree]{.noted}[^conservative]. The external validity claim then becomes something like "Study A gives us reason to believe that intervention B---which worked on population C at time and place D---will also work on the right population E at the right time and place F." But this is pretty tautological. To be a useful tool, external validity should transform a hard problem into a simpler one. But it turns out that, once we unpack things, it's hard to know what "similar" means other than "right" and we're back where we started---we have to rely on outside knowledge to know if we can translate "It worked there." to "It will work here."

## The <i>Evidence-based Policy</i> solution

<blockquote>
To get a good argument from "it works somewhere" to "it will work here" facts about causal principles here and there are needed.
</blockquote>

<!--more-->

## Terms

### Causal principle

A [causal principle](#key-terms) provides a reliable, systematic connection between cause and effect. Because the world is complicated, causal principles often don't take on a simple form. Instead, they are characterized by Insufficient but Necessary parts of an Unnecessary but Sufficient part (INUS). If I want to reach a lichess [TODO] ELO of 1500 [TODO], excellent knowledge of openings may necessary but insufficient because I would also need at least passable middlegame and endgame ability to finish out the match [TODO]. However, all those necessary factors (excellent opening, passable middlegame and endgame [TODO]), even if collectively sufficient, aren't collectively necessary---I could take an entirely route to 1500 by overcoming mediocre opening play with excellent middlegame play or by learning how to cheat with a chess engine. [Schematically]{.noted}[^schema], this looks like a Boolean formula: [(A AND B AND C) OR (B AND D) OR (E AND F AND G) or H]{#boolean-formula} where A, B, C, etc. can be either true or false. In order for the whole proposition to be true, one of the disjuncts [TODO] in parentheses must be true. That means each disjunct (e.g. A AND B AND C) corresponds to an unnecessary but sufficient part. In order for any given disjunct to be true, each of its atoms must be true so each atom (e.g. A) corresponds to an insufficient but necessary part.

### Causal factor

<abbr title="Evidence-based Policy">EBP</abbr> calls each one of the atoms in the Boolean [TODO] formula---each feature of the world that the causal principle specifies as important---a [causal factor](#key-terms). So, to go back to our example, excellent opening play, passable middlegame and endgame, ability to cheat with a chess engine, etc. are all causal factors. 

![Diagram labeling key terms on chess example](/images/evidence-based-policy/chess.svg)

![Diagram labeling key terms on boolean formula example](/images/evidence-based-policy/boolean.svg)


### Causal role and support factors

<abbr title="Evidence-based Policy">EBP</abbr> then goes on to emphasize [causal roles](#key-terms) and [support factors](#key-terms)---a distinction I initially found somewhat confusing. However, my current understanding is that they are really both ways of talking about causal factors (atoms). Any given intervention focuses on one or a few causal factors. <abbr title="Evidence-based Policy">EBP</abbr> describes these causal factors that are the subject of manipulation as having a causal role. The causal factors specified by the causal principle that aren't the focus of the intervention (but are jointly necessary with the focused causal factor) are deemed support factors. So whether we talk about a causal factor with the language "causal role" or with the language "support factor" depends only on the focus of intervention. [We can interpret "X plays a causal role" as "X is a causal factor manipulated by our intervention" and "Y is a support factor" as "Y is a necessary causal factor not manipulated by our intervention".]{.noted}[^confused]

In our chess example, if I started memorizing openings from an opening book, I would be aiming to make my openings play a positive causal role. The other causal factors like passable middle and endgame---while considering this opening intervention---would be support factors. Learning how to cheat with a chess engine is a causal factor in the overall causal principal, but it's not a support factor because it's not relevant to the intervention we're focusing on. In our Boolean formula, if I'm trying to change A from 'false' to 'true', I'm hoping to make 'A' play a positive causal role. While focusing on A, I consider B and C to be support factors. D, E, F, G and H are causal factors but not support factors for A.

## Thesis

Now that we've [defined the key terms of <abbr title="Evidence-based Policy">EBP</abbr>]{.noted}[^defined], we can present the thesis of <abbr title="Evidence-based Policy">EBP</abbr>. The thesis is that to feel confident that "It will work here." based on evidence that "It worked there." requires an argument like this:

<blockquote>
Effectiveness argument

1. The policy worked there (i.e., it played a positive causal role in the causal principles that hold there and the support factors necessary for it to play this positive role there were present for at least some individuals there).
2. The policy can play the same causal role here as there.
3. The support factors necessary for the policy to play a positive causal role here are in place for at least some individuals here post-implementation.

Conclusion. The policy will work here.
</blockquote>

## Making the effectiveness argument

If we have a efficacious intervention ("It worked there."), how can we fill in the rest of the effectiveness argument above? <abbr title="Evidence-based Policy">EBP</abbr> advocates for what they call [vertical](#key-terms) and [horizontal search](#key-terms).

### Vertical search

Vertical search is about finding the right formulation for [a causal subprinciple]{.noted}[^subprinciple]. Any putative causal subprinciple can be stated at different levels of abstraction. In order of increasing abstraction, we can have causal subprinciples like:

1. This hammer claw makes it possible to extract this nail.
2. Hammer claws make it possible to extract nails.
3. Levers make it possible to transform a small force exerted over a large distance into a large force exerted over a small distance. 
4. Simple machines make it possible to do things we otherwise couldn't via mechanical advantage. [TODO]

Even if all these formulations are true, some are more useful than others. 1 is too specific to be of much use. 2 is useful in cases just like ours while 3 provides insights and may allow us to generalize to novel situations. 4 may be too abstract and requires expertise to turn into practical advice---if you're struggling to pull out a nail and someone offers the advice that "Simple machines provide mechanical advantage.", you may not be eternally grateful for their sage advice.

It's worth making explicit here that it won't always be obvious how generalizable causal subprinciples are. In the [vignette](#vignette) at the beginning, it might have sounded eminently plausible that the causal subprinciple to be learned from the successful TINP program was "If we increase the nutritional knowledge of mothers, then childhood malnutrition will be reduced." It's only after the BINP program---designed on this principle---failed that it became apparent that the principle is better phrased as "If we increase the nutritional knowledge of those in charge of childhood nutrition, then childhood malnutrition will be reduced." In Tamil Nadu, these two principles are the same. It's only in Bangladesh where fathers and mothers-in-law play a more important role in childhood nutrition that these two principles diverge and finding the correct level of abstraction becomes crucial.

So generally there's a tension between making causal subprinciples concrete enough to be useful and abstract enough to be true in new circumstances. This is why <abbr title="Evidence-based Policy">EBP</abbr> advocates careful vertical search up and down the ladder of abstraction.

### Horizontal search

Horizontal search is about identifying the full set of support factors necessary for an intervention to succeed. Because support factors aren't the target of an intervention, it's easy to miss them. But if they're missing, the intervention will fail all the same.

For example, in childhood malnutrition interventions like TINP and BINP, careful search could uncover additional support factors like:

- There's an adequate amount of food available for purchase (if not dealing with subsistence farmers)
- It's possible to construct a nutritious diet with locally available foods
- The parasite load is low enough that a sufficient quantity of nutrients can be absorbed [TODO]

## In practice

To its credit, <abbr title="Evidence-based Policy">EBP</abbr> recognizes that the alternative procedure it advocates is not a drop-in replacement. Devolution [TODO] and discretion will be central to the new world order. Tendentiously, the book would have us supplant a mechanical method (RCTs, clearinghouses, external validity) with a much fuzzier one demanding more expertise and discretion of policymakers. But, <abbr title="Evidence-based Policy">EBP</abbr> argues, this is change is necessary because the mechanical method simply isn't up to the task.

<blockquote>
What’s wrong with the ideas of external validity and similarity is that they invite you to stop thinking. [...] We do not think that it is possible to produce unambiguous rules for predicting the results of social policies. So, we do not think that we can produce these rules. So in our world, those who make the decisions will have to deliberate using their judgment and discretion.
</blockquote>

# Efficacy

Though the book is mainly about the problem of [effectiveness](#key-terms) it also has a section on [efficacy](#key-terms). In particular, it talks about causal inference using RCTs and alternatives.

## Randomized controlled trials

RCTs are the holy grail for causal inference (well, meta-analyses and/or systematic reviews of RCTs). <abbr title="Evidence-based Policy">EBP</abbr> proposes that this is because RCTs are "self-validating". By this, they mean that RCTs are something of a magic causality black box---perform the right ~~rituals~~ procedures and out pops a causal claim. There's no need to have a detailed understanding of mechanism, context, population or anything at all in the domain.

Most of the alternative mechanisms of causal inference require real subject matter expertise. Some of these alternatives include:

[Controlling for confounds](https://en.wikipedia.org/wiki/Confounding#Control)
:   You must know the full set of confounds to control for them. This is hard [@gordon2019comparison].

[Instrumental variables](https://en.wikipedia.org/wiki/Instrumental_variables_estimation)
:   You must know that there's no other causal linkage between your instrument and your outcome variable.

[Causal graph](https://en.wikipedia.org/wiki/Causal_graph)
:   You must know the [full]{.noted}[^full] causal structure.

So what sets RCTs apart is not their ability to make causal claims---it's that they can do so without expert domain knowledge. But <abbr title="Evidence-based Policy">EBP</abbr> argues that expert domain knowledge will be required anyway to make claims of effectiveness. If expert domain knowledge is required to assess effectiveness, we might as well desanctify RCTs and allow other techniques when assessing efficacy. (At least, I think this is a reasonable connection to make. <abbr title="Evidence-based Policy">EBP</abbr> doesn't make this connection quite as explicit.)

# Omitted

These are the things I've left out of the summary (and remembered to note here---I don't guarantee exhaustiveness, blog name to the contrary).

## What is an argument?

There's a whole section describing warrants, evidence, the structure of arguments, etc. I didn't need to read this and I assume that's true for most other readers of the book or this summary.

## Causal cakes

One of the central explanatory metaphors in <abbr title="Evidence-based Policy">EBP</abbr> is a "causal cake". I omitted this because I found it supremely unhelpful. There's no real reason to read it, but here is a list of my complaints: 

* The book admits "The cake is just a picture of the list [of causal factors].". 
* The book says that it prefers a cake-and-ingredient metaphor to a pie-and-slice metaphor because each of the ingredients is integral to a cake while you still have a lot of pie left if you take out one slice. All the images of causal cakes have them sliced up into wedges in clear contradiction of the aforementioned rationale.
* Things become a bit dizzying when <abbr title="Evidence-based Policy">EBP</abbr> starts talking about "activating" one of the multiple cakes that make up a causal principle. The metaphor feels a bit thin---like ~~frosting~~ butter [TODO] scraped over too much ~~cake~~ bread.

## How to think

A whole chapter of <abbr title="Evidence-based Policy">EBP</abbr> is devoted to what I'd say are fairly general tools for thinking. It lists four strategies intended to help in constructing a robust effectiveness argument. However, since the chapter is not 1) integral to the rest of the book, 2) an exhaustive listing of all such thinking tools, 3) especially revelatory, I have not covered it in depth here. The four strategies mentioned are:

* pre-mortem 
* thinking step-by-step and thinking backwards
* "If it is to work, by what means will it do so?"
* quick exit trees (a variant of decision trees)

## Evidence-ranking schemes

<abbr title="Evidence-based Policy">EBP</abbr> also has a full section on evidence-ranking schemes and policy clearinghouses like the [What Works Network](https://www.gov.uk/guidance/what-works-network). While I think these are interesting and valuable resources, the discussion doesn't seem essential to the <abbr title="Evidence-based Policy">EBP</abbr>'s core thesis.

## Fidelity

The section on fidelity when implementing an efficacious intervention doesn't seem to cover much that wasn't already discussed when talking about vertical search and external validity.

Raj Chetty's econ revamp vox article [TODO]


[^conservative]: Also, external validity's demand for "similar" is too conservative. It's possible to imagine contexts that differ in some intervention-relevant way where you'd still be happy to repeat the intervention. You probably shouldn't say: "Oh, sorry, can't do it. Direct cash transfers of $50 a month are only helpful for those with an annual income of $2,000. They wouldn't work for those with an annual income of $1,000. $1,000 isn't similar to $2,000---it's only half as much." Ultimately, what we want is not a "similar" context but a context which is at least as favorable for the intervention.
[^schema]: Of course, almost all real world causal principles can't be expressed as boolean formulas [TODO] this simple. I've chosen a simplified example for pedagogical purposes and hope you can see or trust that similar dynamics arise with more complicated causal principles.
[^confused]: Perhaps my initial confusion now makes sense to you. Support factors also play a causal role by the plain meaning of "causal role"---if they were counterfactually absent, a different effect would result. <abbr title="Evidence-based Policy">EBP</abbr> just seems to have imbued the phrase "causal role" with a special (and, in my opinion, confusing) meaning.
[^defined]: Annoyingly, <abbr title="Evidence-based Policy">EBP</abbr> offers no succinct, upfront definition of "causal role" that I can find (Believe me, I've looked).
[^subprinciple]: <abbr title="Evidence-based Policy">EBP</abbr> uses the term "causal principle" in two different ways. The first is the way we've already outlined---a specification of the full (possibly disjunctive) causal structure responsible for an effect. The second usage of "causal principle" is to describe the relationship between one set of jointly sufficient causes and their effect. To avoid confusion, I use the term "causal subprinciple" for this second concept. 

    To be more explicit, a causal principle governs causal factors A through H in our Boolean formula. A causal subprinciple governs any of the disjuncts like (A AND B AND C). A causal principle governs all the causal factors about reaching an ELO of 1500 while a causal subprinciple governs all the causal factors about reaching 1500 via improving knowledge of openings with a passable middle and endgame.
[^full]: Where "full" means "any additional variable that either directly or indirectly causally affects at least two variables already included in the DAG should be included" [@rohrer2018thinking].

<hr class="references">
