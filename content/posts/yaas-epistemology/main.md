---
title: YAAS epistemology
subtitle: "Yet Another Amateur Summary:<br>Crumley's <i>An Introduction to Epistemology</i>"
date: 2018-10-01
tags: yaas, epistemology
graph-of-contents: yaas-epistemology
css: yaas-epistemology
---

<menu id="just-explain" type="popup">
  <menuitem label="Anecdotal" type="radio" checked="checked"></menuitem>
  <menuitem label="Schematic" type="radio"></menuitem>
  <menuitem label="Graphical" type="radio"></menuitem>
</menu>

Epistemology is the study of [knowledge](#yaas-epistemology-map){#knowledge .arg-map}.

# [What is knowledge?](#yaas-epistemology-map){#what .arg-map}

For literally millenia, people were content to think of knowledge as justified true belief [@plato360]. Let's break that down briefly:

Belief
:   A belief is mental content held as true. Thinking to myself, with conviction, "I am a human." and "I am a dragon." are each equally examples of beliefs.

True
:   Alas, only one of the above claims is true. Even if I have very convincing hallucinations of breathing fire and flying, "I am a dragon" is merely justified belief. It's not true and so not knowledge.

Justified
:   On the other hand, if I stumble onto true belief, it still doesn't count as knowledge. If you roll a 100-sided die and hide the result under a cup, I don't *know* that the result is 64 even if I believe it and it turns out to be true---I have no justification for the belief.

It's only when beliefs are both true *and* justified that they count as knowledge. Sounds reasonable, right?

## [Gettier problem](#yaas-epistemology-map){#gettier .arg-map}

Not to Edmund Gettier. In a three page paper [@gettier1963], he presented two compelling counterexamples to this analysis. An example of this vein of anecdote is:

Whetu is driving home from work one day and happens to see her coworker Sigrún in a [Vanagon](https://en.wikipedia.org/wiki/Volkswagen_Type_2_(T3)). She thinks to herself, "Ah, one of my coworkers owns a Vanagon." However, unbeknownst to Whetu, Sigrún does not actually own the Vanagon she was driving---she was in the midst of stealing it. But, coincidentally, one of Whetu's other coworkers *does* own a Vanagon.

So Whetu's initial belief---"One of my coworkers owns a Vanagon"---turns out to be true and justified (we'll count seeing someone driving a car as convincing justification for believing that they own it). But intuitively, we're reluctant to accept that Whetu knows that one of her coworkers owns a Vanagon. It seems like she just got lucky---her mistaken justification was rescued by a fact that she would be surprised to learn.

<!--more-->

This set off a frenzy of philosophical activity that has yet to cease [@shope2017]. Most philosophers try to repair the traditional analysis by finding some appropriate link between justification and truth. If Whetu just got lucky, we need to ensure that luck doesn't suffice to constitute knowledge. For reasons of brevity (among others), I'll direct the interested to [SEP](https://plato.stanford.edu/entries/knowledge-analysis/) for a further discussion of the reams of argumentation on this topic.

## [Justification](#yaas-epistemology-map){#justified .arg-map}

Let's circle back to justification and look at it more closely.

### [Münchhausen trilemma](#yaas-epistemology-map){#munch .arg-map}

::: {.switch type=menu data-menu=just-explain}
::: open
Suppose you're with an inquisitive, relentless, precocious, philosophically convenient child in the park. They turn to you and ask:

::: conversation

"What shape is that cloud?"

"Square."

"Why do you believe that?"

"Because I see it with my eyes."

"Why do you believe your eyes?"

"Because they've historically been reliable."

"Why do you believe your eyes have historically been reliable?"

:::

Now, we have a choice.

[Infinitism](#yaas-epistemology-map){#infinit .arg-map}
:   It seems clear that we may well face an endless stream of "why"s. We can choose to accept this and generate an endless series of responses, to the best of our ability:

    ::: conversation

    "Why do you believe your eyes have historically been reliable?"

    "Because last time I believed that my eyes were reliable, I met with success."

    "Why did you believe that your eyes were reliable last time?"

    "Because the time before that, I believed my eyes were reliable and met with success.

    ...

    :::

    Endorsing infinite chains of justification characterizes [infinitism](https://www.iep.utm.edu/inf-epis/).

[Foundationalism](#yaas-epistemology-map){#found .arg-map}
:   Alternatively, we can try to stem the stream of "why"s. One way is bare assertion:

    ::: conversation

    "Why do you believe your eyes have historically been reliable?"

    "Because they agree with my other senses and my other senses have historically been reliable."

    "Why do you believe your other senses are reliable?"

    "I just do. I have to have some kernel beyond doubt."

    :::

    Endorsing justification by some set of basic beliefs which are themselves justified without reference to other beliefs characterizes [foundationalism](https://plato.stanford.edu/entries/justep-foundational/).

[Coherentism](#yaas-epistemology-map){#coherent .arg-map}

:   Finally, we can try to avoid an infinite stream of "why"s by justifying one belief with another belief we've already justified:

    ::: conversation

    "Why do you believe your eyes have historically been reliable?"

    "Because they agree with my other senses and my other senses have historically been reliable."

    "Why do you believe your other senses have historically been reliable?"

    "Because they agree with my eyes and my eyes have historically been reliable."

    :::

    Endorsing cyclic justification---we justify sight with reference to other senses and other senses with reference to sight--- characterizes [coherentism](https://plato.stanford.edu/entries/justep-coherence/).

So, to summarize, when faced with endless 'why's, we can pursue one of three strategies: Provide an endless stream of responses, declare that some beliefs are beyond questioning, or provide circular justification. Again, there's lots of arguments about which (if any) of these positions is correct which we'll not cover here.
:::
::: {}
In general, each belief ought to have some justification. Beliefs without any justification aren't even candidates for counting as knowledge. But the justification for a belief is also a belief and thus demands *it's* own justifying belief.

In other words:

To be justified in believing proposition P on the basis of evidentiary proposition E, one must be justified in believing E.

For example, to be justified in believing the proposition P that you weigh 140 pounds, you'd have to be justified in believing the proposition E that your scale is accurate.

If we apply this rule unconditionally, we have an infinite stream of justifications. P is justified by E. E is justified by F. F is justified G. <i>Ad infinitum</i>. Endorsing an infinite stream of justifications as epistemically right and proper characterizes [infinitism](https://www.iep.utm.edu/inf-epis/).

If, instead, we say that some rules can be justified without respect to any evidentiary propositions E, we avoid an infinite stream of justifications. P is justified by E. E is justified by F. F is foundational and requires no justifying beliefs. Endorsing beliefs which are not justified by other beliefs characterizes [foundationalism](https://plato.stanford.edu/entries/justep-foundational/).

Finally, if having a blessed set of foundational beliefs is unappealing, we can avoid an infinite chain of justification by allowing cyclic justification. P is justified by E and E is justified by P. Allowing cycles of justification characterizes [coherentism](https://plato.stanford.edu/entries/justep-coherence/).

Which of these positions is correct? Who knows! Let someone else somewhere else decide. We're moving on.
:::
::: {}
We can think of propositions as vertices in a graph and justifications as edges between these vertices.

If we insist that beliefs are only justified when supported by some more basic belief we end up with a graph that looks like this:

<figure class="infinitism">
![A linear, infinite, directed graph of belief vertices connected by justifying edges](/images/yaas-epistemology/infinitism.svg)
<figcaption>A graph representing [infinitism](https://www.iep.utm.edu/inf-epis/)</figcaption>
</figure>

If we permit some beliefs to be "foundational"---not justified by other beliefs---we end up with a graph like:

<figure class="foundationalism">
![A linear, directed graph of belief vertices connected by justifying edges which terminates in a foundational belief](/images/yaas-epistemology/foundationalism.svg)
<figcaption>A graph representing [foundationalism](https://plato.stanford.edu/entries/justep-foundational/).</figcaption>
</figure>

If we permit some beliefs to be mutually justifying, we end up with a graph like:

<figure class="coherentism">
![A directed graph of belief vertices connected by justifying edges which terminates in a cycle](/images/yaas-epistemology/coherentism.svg)
<figcaption>A graph representing [coherentism](https://plato.stanford.edu/entries/justep-coherence/).</figcaption>
</figure>

Argumentation around which of these approaches is correct is out of scope for this blog post.
:::
:::

### [Internalism v. externalism](#yaas-epistemology-map){#internal-external .arg-map}

All this talk about the structure of justification probably raises the question: What *is* justification? There's surprisingly little agreement. Philosophers have staked out some views though.

One key disagreement is between [externalist and internalist accounts of justification](https://www.iep.utm.edu/int-ext/). Internalists hold that all the features relevant for justification are "internal" to a person. Externalists hold that factors "outside" of an individual can also be relevant to the question of whether that person's belief is justified. Internal factors are things like beliefs and other mental content we can access upon reflection. External factors are things like the process by which we arrived at those beliefs---we often don't recall or know the mechanism by which we arrived at some arbitrary belief.

For example, suppose you believe that your aunt is in the same province as you. You believe this because you believe that she's on a vacation there. You believe your aunt is on a vacation to your province because a [tarot deck](https://en.wikipedia.org/wiki/Tarot#Occult_tarot_decks) said so. An internalist is closer to being obliged to call the original belief (about your aunt being in the same province) justified because the structure of beliefs---internal mental content---in the immediate vicinity is proper. An externalist has additional grounds on which they can reject this as valid justification---they can object that a tarot deck is not a reliable method for forming beliefs and so not a firm ground for justification. The internalist, on the other hand, can make no appeals to the (un)reliability of tarot cards because it's a claim about the world at large, not the internal mental state of the believer.

Yet again, there's lots of argument on both sides of this debate. We can't possibly do it justice here. It is interesting to note though that a solid plurality of surveyed philosophers favored externalism [@bourget2014]:

<figure>
<figcaption>Epistemic justification: internalism or externalism?</figcaption>
| Position                             | Responses   |   Percentage |
| :----------------------------------- | ----------: | -----------: |
| Accept or lean toward: externalism   | 398 / 931   |        42.7% |
| Other	                               | 287 / 931   |        30.8% |
| Accept or lean toward: internalism   | 246 / 931   |        26.4% |
</figure>

### [The problem of induction](#yaas-epistemology-map){#induction .arg-map}

[Induction](https://en.wikipedia.org/wiki/Inductive_reasoning) (not the [mathematical sort](https://en.wikipedia.org/wiki/Mathematical_induction)) is about generalizing from particulars to generalities not logically entailed by those particulars. For example, I've seen a dozen black crows in a dozen different places and on this basis conclude that all crows are black.

We perform this sort of operation all the time but on what warrant? Our (often implicit) belief in the validity of induction relies on what we'll call the uniformity principle:

<blockquote>
instances, of which we have had no experience, must resemble those, of which we have had experience, and that the course of nature continues always uniformly the same. [[@hume1739]]{.attribution}
</blockquote>

But [how can we justify belief in such a proposition](https://plato.stanford.edu/entries/induction-problem/)? It does not seem to be a logical inevitability. And on the other hand, it doesn't seem that any finite set of empirical observations elevates it above doubt. Even if the laws of physics have applied uniformly over the past century, that's little guarantee that they'll proceed on placidly forevermore. Any attempt to generalize particular instantiations of the uniformity principle into a universal affirmation of the uniformity principle would *itself* rely on the uniformity principle.

### [<i>A priori</i> and <i>a posteriori</i>](#yaas-epistemology-map){#priori-posteriori .arg-map}

A proposition is justified [<i>a priori</i>](https://www.iep.utm.edu/apriori/) if it is [justified without reference to any sense experience]{.noted}[^a-priori]. If the justification relies on sense experience, a proposition is justified <i>a posteriori</i>.

For example, our belief in the proposition "All bachelors are unmarried." isn't contingent on any particular sense experience. Even if I'd lived my whole life apart from human society and had never seen any other humans---bachelor or otherwise---I'd readily assent to the proposition as soon as I understood the terms involved.

Conversely, the proposition "It's chilly outside." is intimately connected to sense experience. It is sometimes true and sometimes false and which case obtains ultimately depends on sense data. If I go outside and feel the cool air on my skin, it may well change my disbelief to belief.

Should we care about this distinction or is its continued regurgitation just ritual genuflection before our lord and savior Immanuel Kant? I make no definite claims on the matter here, but I do hope that the distinction seem a bit more interesting in light of the next section.

## [Belief](#yaas-epistemology-map){#belief .arg-map}

Closely related to the notions of <i>a priori</i> and <i>a posteriori</i> justification are [the notions of necessary and contingent propositions, and analytic and synthetic propositions]{.noted}[^belief-cute]. All were addressed prominently in [@kant1781].

### [Necessary and contingent](#yaas-epistemology-map){#modal .arg-map}

A proposition is [necessarily true](https://plato.stanford.edu/entries/modality-varieties/) if its negation is impossible or contradictory. That is, the proposition is true in all [possible worlds](https://plato.stanford.edu/entries/possible-worlds/). Conversely, a proposition is contingently true if being both true without contradiction and false without contradiction. That is, the proposition is true in some possible worlds and false in others.

For example, the proposition "All bachelors are unmarried." is necessarily true. Given a fixed meaning for the terms, there is no possible world in which the proposition is false. On the other hand, "It's chilly outside." is true in some possible worlds and false in others---it is contingent.

Hopefully, the following claims come as no surprise to you:

- Contingent propositions are closely related to <i>a posteriori</i> justification. If a proposition is true in some possible worlds and false in others, we quite plausibly have need of our senses to determine which possible world we actually inhabit and, correspondingly, whether the proposition in question is true or false.
- Necessary propositions are closely related to <i>a priori</i> justification. If a proposition is true in all possible worlds, the task of determining which world we actually inhabit is otiose when it comes to determining the truth of the proposition. Since we don't care which world we actually inhabit, our sense experience is irrelevant to the task at hand.

Alas, the relationship between these categories isn't actually as straightforward as all that. There's ongoing disagreement about the precise nature of the relationship. In fact, there's not even universal acknowledgment that <i>a priori</i> knowledge exists [@bourget2014]:

<figure>
<figcaption><i>A priori</i> knowledge: yes or no?</figcaption>
| Position                     | Responses   |   Percentage |
| :--------------------------- | ----------: | -----------: |
| Accept or lean toward: yes   | 662 / 931   |        71.1% |
| Accept or lean toward: no    | 171 / 931   |        18.4% |
| Other                        | 98 / 931    |        10.5% |
</figure>

### [Analytic and synthetic](#yaas-epistemology-map){#ana-synth .arg-map}

The final Kantian distinction is between [analytic and synthetic propositions](https://plato.stanford.edu/entries/analytic-synthetic/). An analytic proposition is one whose meaning is true in light of the meaning its terms. A synthetic proposition is one whose meaning is true in light of the correspondence between its meaning and the world. "All bachelors are unmarried." is an analytic proposition and "It's chilly outside." is a synthetic proposition.

Hopefully, that distinction is fairly straightforward at this point (If not, you can always follow the link above.). With it out of the way, we can move on to talk about a slightly more complex taxonomy:

<figure>
<figcaption>The relationship between analytic-synthetic and <i>a priori</i>-<i>a posteriori</i></figcaption>
|           | <i>A priori</i>                       | <i>A posteriori</i>                     |
|:----------|:--------------------------------------|:----------------------------------------|
| [Analytic]{.label-row} | Logical propositions; Mostly accepted | Mostly rejected                         |
| [Synthetic]{.label-row} | Empiricists v. rationalists           | Empirical propositions; Mostly accepted |
</figure>

There's general philosophical agreement that analytic propositions with <i>a priori</i> justification exist, that synthetic propositions with <i>a posteriori</i> justification exist, and that analytic propositions with <i>a posteriori</i> justification don't exist. The most controversial intersection then is between <i>a priori</i> and synthetic. On one view, this is what the [rationalism v. empiricism](https://plato.stanford.edu/entries/rationalism-empiricism/) debate is about---rationalists hold that synthetic <i>a priori</i> knowledge is possible while empiricists deny it.

That also serves as the segue for the shoehorning of two more survey results [@bourget2014]:

<figure>
<figcaption>Analytic-synthetic distinction: yes or no?</figcaption>
| Position                   | Responses | Percentage |
|:---------------------------|----------:|-----------:|
| Accept or lean toward: yes | 604 / 931 |      64.9% |
| Accept or lean toward: no  | 252 / 931 |      27.1% |
| Other                      | 75 / 931  |       8.1% |
</figure>

<figure class="empiri-rati">
<figcaption>Knowledge: empiricism or rationalism?</figcaption>
| Position                           | Responses | Percentage |
|:-----------------------------------|----------:|-----------:|
| Other                              | 346 / 931 |      37.2% |
| Accept or lean toward: empiricism  | 326 / 931 |      35.0% |
| Accept or lean toward: rationalism | 259 / 931 |      27.8% |
</figure>

## [Truth](#yaas-epistemology-map){#true .arg-map}

Your patience with these persnickety philosophers likely begins to grow thin. They cavil and niggle, carp and quibble. They've taken common sense notions like knowledge, belief and justification, raised questions, drawn distinctions and offered precious little in the way of answers. But at last, you think, we come to truth---a concept so straightforward it's beyond the grasping claws of even the most wretched sophist. Alas...

Philosophers advance myriad interpretations of truth. These can be categorized under a few broad headings:

[Correspondence](#yaas-epistemology-map){#correspondence .arg-map}

:   [Correspondence theories of truth](https://plato.stanford.edu/entries/truth-correspondence/) are probably closest to common sense, folk theories of truth. They say that a proposition is true if and only if it corresponds to the world---if the map corresponds to the territory.

[Epistemic](#yaas-epistemology-map){#epistemic .arg-map}

:   [Epistemic theories of truth](https://en.wikipedia.org/wiki/Epistemic_theories_of_truth) are a heterogeneous lot. The common element is that they all suggest that the truth of a proposition depends on our beliefs. For example, the [[coherence theory of truth](https://plato.stanford.edu/entries/truth-coherence/)]{.noted}[^coherence] holds that a proposition is true in light of coherence with other propositions.

[Deflationary](#yaas-epistemology-map){#deflationary .arg-map}

:   [Deflationary theories of truth](https://plato.stanford.edu/entries/truth-deflationary/) are a bit hard to succinctly explain. We'll just say that, like it says on the tin, they try to take the magic out of 'truth' and get by with a minimal notion.

Here too, the debate is unresolved [@bourget2014]:

<figure>
<figcaption>Truth: correspondence, deflationary, or epistemic?</figcaption>
| Position                              | Responses | Percentage |
|:--------------------------------------|----------:|-----------:|
| Accept or lean toward: correspondence | 473 / 931 | 50.8%      |
| Accept or lean toward: deflationary   | 231 / 931 | 24.8%      |
| Other                                 | 163 / 931 | 17.5%      |
| Accept or lean toward: epistemic      | 64 / 931  | 6.9%       |
</figure>

# [Is knowledge possible?](#yaas-epistemology-map){#possible .arg-map}

## [Skepticism](#yaas-epistemology-map){#skeptics .arg-map}

When we presented the [Münchhausen trilemma](#münchhausen-trilemma), we actually omitted one possible response. We said that justifying knowledge ultimately required coherentism, foundationalism, or infinitism. The fourth option is to deny the possibility of knowledge altogether. Without any knowledge at all, nothing stands in need of justification and the whole problem is neatly avoided. Denying the possibility of knowledge characterizes [skepticism](https://plato.stanford.edu/entries/skepticism/).

One thought experiment that generates an intuition for skepticism is [the dream argument](https://en.wikipedia.org/wiki/Dream_argument): When we dream, we usually don't know we're dreaming. How do we know we're not dreaming now? How can we have complete certainty that our perceptual experiences relate to the real, physical world rather than a dream world?

## [Fallibilism](#yaas-epistemology-map){#fallibilists .arg-map}

In some ways, [fallibilism](https://www.iep.utm.edu/fallibil/) is the softer cousin of skepticism. Like skepticism, it denies that we can ever make claims with absolute conviction; we can never be certain we know the truth. Unlike skepticism, it is content to call fallibly justified beliefs---which may turn out to be false---knowledge.

## [Epistemism](#yaas-epistemology-map){#epistemists .arg-map}

Epistemism doesn't seem to be a term in common use, but, for the sake of completeness, we'll use it to describe the position that says we can have certain knowledge.

# Omitted

In additional to all the details we've skated over, there are whole subtopics covered in [@crumley2009] that we've omitted here. These include:

- Perception
- Feminist epistemology
- Naturalized epistemology

<hr class="references">

[^a-priori]: Of course, humans can never actually come to hold beliefs independent of all sense experience. Our lives are filled with sensory experiences and before believing a proposition, we must come to understand the proposition and the language used to express it (a process which necessarily relies on our senses). The <i>a priori</i>-<i>a posteriori</i> distinction is not about the causal mechanism by which we come to believe a proposition; it's about the epistemic justification for a proposition.

[^belief-cute]: Putting these under "Beliefs" is a bit cute---they're more properly regarded as describing propositions. But the contents of beliefs are propositions and this fits into our overarching organizational structure nicely.

[^coherence]: Note that this is not the same as [coherentist theories of epistemic justification](https://plato.stanford.edu/entries/justep-coherence/).
