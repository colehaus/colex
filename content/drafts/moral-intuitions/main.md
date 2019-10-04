---
title: "The Reliability of moral intuitions: A systematic review"
date: 2019-10-03
tags: ethics, statistics
include-toc: true
---
# Intro

## What are moral intuitions? 

By "[moral intuition]{.noted}[^intuition-def]", we will mean an immediately available sense of the moral status of some proposition---a sense that is available without extensive explicit or formal reasoning. If asked "Is it wrong for David to hurt an innocent child for no reason other than David's pleasure at doing so?" [TODO], your reflexive answer is (presumably) "Yes" without any need to appeal to particular ethical theories or to explicitly weight costs and benefits. Philosophical intuitions are sometimes analogized to sensory perception---both perception and philosophical intuition can produce strong sensations of tracking an independent reality but their mechanisms of operation are fairly opaque.

[^intuition-def]: In the literature, there are a variety of subtle variations in the definitions and descriptions of philosophical intuition, but they all seem to be clustered around a concept like the one described above. Furthermore, I don't think of any of the subsequent work hinges on the precise definition so we'll not get too fussed about it [TODO].

## Why do we care about moral intuitions?

The basic answer is that they seem to play a crucial role in multiple types of moral reasoning.

One possible mode of moral reasoning is simply to rely upon moral intuitions as the core and perhaps sole ingredient of moral judgments. If your moral intuition says that stealing a loaf of bread is wrong, that's pretty much the beginning and the end of it. You don't have to consider the consequences of the action or whether you will that it become a universal maxim [TODO]. Since most people aren't familiar with explicit ethical systems (e.g. utilitarianism, Kantianism, virtue theory) as found in philosophy [CITATION NEEDED] [TODO], they certainly cannot be said to knowingly deploy those systems when faced with moral conundra [TODO]. (Note that parsimony does not mean that systems of moral reasoning dominated by intuition are simple or bad. See, for example, Nucci's and Turiel's domain theory [TODO], Haidt's social intuitionist model and later moral foundations theory [TODO], Greene's dual process theory [TODO] and the theory of universal moral grammar [TODO] for various accounts of all that goes in to moral intuitions behind the scenes.) But perhaps, you object, even in the absence of theories endorsed by the hoi polloi [TODO], people are applying systematized folk theories of morality. Your meritorious objection provides a convenient segue.

Another mode of moral reasoning is using moral intuitions as inputs to some "higher level" moral theory. From a folk morality perspective, this might look like mastering your visceral disgust reaction upon hearing that a brother and sister passionately kiss each other on the mouth and then reminding yourself that such behavior is consensual and (potentially) harmless. When philosophers engage in this bipartite [TODO] process, they talk about "reflective equilibrium" [TODO; Rawls, philosophical methodology]. Reflective equilibrium is about: 

1. Feeding certain moral intuitions into a moral rule
2. Finding the implications of that rule applied to those intuitions
3. Determining whether those implications are acceptable i.e. whether bullets will be bit
4. If the implications aren't acceptable, changing either the moral rule endorsed in step 1 or the set of moral intuitions endorsed in step 1
5. Repeating steps 1 through 4 until the implications of endorsed intuitions and rules are acceptable i.e. reach equilibrium

More succinctly, reflective equilibrium is about bouncing back and forth between intuitions and rules until your intuitions dictate rules you can accept and your rules dictate intuitions you can accept. At any rate, the point in all this is that even in these more involved processes of moral reasoning, moral intuitions play a key role. There's no practical ethical theory that is purely formal and entirely ungrounded in moral intuitions.

(This reflective equilibrium perspective also suggests a slightly different definition of moral intuition from the ones discussed above. We could just call moral intuitions "the data fed into higher level moral theories" which is perhaps closest to what we truly care about when considering reliability; we ultimately care about the reliability of the system of moral reasoning as a whole more than the reliability of any arbitrary subcomponent.)

## What would it mean for a moral intuition to be unreliable?

"moral intuitions are unreliable to the extent that morally irrelevant factors affect moral intuitions." [@Moral psychology handbook/Moral intuitions/TODO]

These influential but morally irrelevant factors (all attested [TODO] to varying degrees in the literature as we'll see below) include things like: 

Order
:   The moral acceptability of a vignette depends on the order in which it's presented relative to other vignettes.
Wording
:   The moral acceptability of a vignette depends on the wording with which it's presented relative to other vignettes.
Disgust
:   The moral acceptability of a vignette depends how disgusted the moralist (i.e. the person judging moral acceptability) feels at the time.
Gender
:   The moral acceptability of a third-person vignette depends the gender of the moralist [TODO].
Culture
:   The moral acceptability of a third-person vignette depends on the culture of the moralist.
Socioeconomic status
:   The moral acceptability of a third-person vignette depends on socioeconomic status of the moralist.
Personality
:   The moral acceptability of a third-person vignette depends on the personality of the moralist.
Actor/observer
:   The moral acceptability of a vignette depends on whether the vignette is about the actions of the moralist or some third party.

(The claim that certain factors are morally irrelevant is itself part of a moral theory. However, some of these factors seem to be morally irrelevant on a *very* wide range of moral theories. I listed them in rough order from those I factors I judge most likely to be truly morally irrelevant to those which are most plausibly morally relevant. It seems especially troubling that almost all of these factors cause subjects to violate the principle of [extensionality](TODO).)

## Why do we care about the putative [TODO] unreliability of moral intuitions?

In the first mode of moral reasoning---in which moral intuitions play the dominant role in moral judgments, it's obvious why pervasive and severe unreliability poses a problem. Unreliable intuitions means unreliable judgment and individuals will act wrongly while perceiving themselves to act rightly.

In the second mode of moral reasoning---in which moral intuitions are paired with higher level moral rules, pervasive and severe unreliability likely also poses a problem. The slogan here is "Garbage in, garbage out". (While I think "Garbage out" is a real concern, I don't think it's as tautological as many philosophers in this area seem to think. In many domains, including the physical sciences, we work with imprecise instruments and are able to overcome this imprecision through careful design of procedures and statistical competence. In [distributed systems](TODO) settings, we're able to [build reliable systems out of unreliable components](TODO). [Error correcting codes](TODO) are neat. [@TODO])

Of course, it's not yet clear whether the problems with moral intuitions are indeed pervasive and severe. That's what the rest of this post investigates. But this is at least an open question to be taken seriously. In the philosophical literature, the question often goes under the name "restrictionist challenge":

> The [Restrictionist] Challenge, in a nutshell, is that the evidence of the [human philosophical instrument]’s susceptibility to error makes live the hypothesis that the cathedra lacks resources adequate to the requirements of philosophical enquiry. [@TODO]

I think it's also worth noting one particularly aggravating/worrisome [TODO] aspect of the unreliability of moral intuitions. We can think of tools as having a domain of applicability---problems to which they can be applied at all---and a domain of validity---problems to which they can be applied successfully. First best is when the domain of applicability and domain of validity coincide perfectly---there can be no possibility of misuse here. Second best is when the domain of applicability is larger than the domain of validity but the extent of these domains is clear (perhaps only retrospectively). When the tool fails, it's clear. The worst case is when the domain of validity is a subset of the domain of applicability but it's not clear *which* subset. The tool fails silently and there's no easy way to distinguish between valid and invalid results. I fear that moral intuitions may fall into this third case. It seems wholly possible that our moral intuitions are wrong and harmful even when they feel strongest and clearest [@possibility of ongoing moral catastrophe TODO]. If we could move moral intuitions to the second case---knowing when they're wrong or at least unreliable---that would be great [TODO]. But I won't really explore this topic more since this post is already overflowing with words (I will leave a few final references [@wright2010intuitional] [@wright2013tracking].).
