---
title: YAAS Social Norms
subtitle: "Yet Another Amateur Summary: Bicchieri's <i>Norms in the Wild: How to Diagnose, Measure, and Change Social Norms</i>"
date: 2018-11-13
edited: 2018-11-15
tags: yaas, norms
graph-of-contents: yaas-social-norms
series: Norms in the Wild
---

<menu id="norm-problem" type="popup">
  <menuitem label="Status quo preferred" type="radio" checked="checked"></menuitem>
  <menuitem label="No alternative" type="radio"></menuitem>
  <menuitem label="Pluralistic ignorance" type="radio"></menuitem>
</menu>

When agents act or plan to act, they do so on the basis of both beliefs and preferences. Alternatively, we can phrase these as [reasons and passions](https://plato.stanford.edu/entries/hume-moral/). Or we can say that [a reward function and a model of the environment are required for a policy](https://en.wikipedia.org/wiki/Reinforcement_learning).

# Foundation

Before we can analyze collective behavior, we have to make a few foundational distinctions.

## [Preferences](#yaas-social-norms-map){#pref .arg-map}

### [Individual or social](#yaas-social-norms-map){#social-pref .arg-map}

Preferences can be either individual or social preferences. Social preferences are those that "take into account the behavior, beliefs, and outcomes of other people" while individual preferences do not [@bicchieri2016]. (Example to come.) This distinction is important because changing individual preferences can plausibly happen in isolation, one person after another. Changing social preferences on the other hand is more likely to require coordinated group action.

### [Conditional or unconditional](#yaas-social-norms-map){#conditional .arg-map}

Preferences can also be conditional or [unconditional]{.noted}[^unconditional]. Conditional preferences are those that vary with some feature of the environment while unconditional preferences do not. This is an important distinction because a social engineer can change the way conditional preferences manifest by changing the environment while unconditional preferences can only be altered in a direct confrontation.

<abbr title="Norms in the Wild">NitW</abbr> also draws a distinction between prudential and moral preferences. I think these can be viewed as a special case of conditional and unconditional preferences. It seems we can gloss these to preferences which seek to satisfy [instrumental or intrinsic values](https://en.wikipedia.org/wiki/Instrumental_and_intrinsic_value). Preferring not to cheat out of a fear of getting caught is a prudential preference; preferring not to cheat because it's an odious breach of faith is a moral preference. This distinction is important because moral preferences are typically more stable---prudential preferences may change with changing circumstances while moral preferences will not.

### Together

If we put these two axes together, we end up with a classificatory grid like this:

<figure id="preferences-grid">
<figcaption>Classifying different types of preferences</figcaption>
|                             | Individual preferences           | Social preferences                         |
|-----------------------------|----------------------------------|--------------------------------------------|
| [Unconditional]{.label-row} | "I want apples."                 | "I want more apples than you."             |
| [Conditional]{.label-row}   | "I want apples if it is autumn." | "I want apples if my friends want apples." |
</figure>

<!--more-->

## [Beliefs](#yaas-social-norms-map){#beliefs .arg-map}

### [Normative and non-normative](#yaas-social-norms-map){#normative .arg-map}

We can also distinguish between [normative and non-normative beliefs](https://en.wikipedia.org/wiki/Positive_statement#Positive_statements_and_normative_statements) ([ought vs is](https://en.wikipedia.org/wiki/Is%E2%80%93ought_problem)). This distinction is important when discussing norms because demonstrations and other empirical evidence can be used to change non-normative beliefs but are harder to apply to normative beliefs.

### [Social and non-social beliefs](#yaas-social-norms-map){#social-belief .arg-map}

Some beliefs are social and others are not. Social beliefs are "expectations we have about
other people’s behaviors and beliefs" [@bicchieri2016]. This distinction is important for much the same reason as the distinction between social and non-social preferences is important---it determines whether a social engineer has a divisible problem or one that must be tackled at the scale of groups.

## Together

If we put these two axes together, we end up with a classificatory grid like this:

<figure id="beliefs-grid">
<figcaption>Classification of normative/non-normative and social/non-social beliefs</figcaption>
|                                     | Non-social beliefs         | Social beliefs         |
|-------------------------------------|----------------------------|------------------------|
| [Non-normative beliefs]{.label-row} | Factual beliefs            | Empirical expectations |
| [Normative beliefs]{.label-row}     | Personal normative beliefs | Normative expectations |
</figure>

To be explicit, empirical expectations are "beliefs about how other people are going to act or react in certain situations" while normative expectations are "beliefs about other people’s personal normative beliefs (i.e., they are second-order beliefs)" [@bicchieri2016]. That is, "All do it" is an empirical expectation while "All approve of it" is a normative expectation.

# [Collective behavior](#yaas-social-norms-map){#collective .arg-map}

With those distinctions made, we can start to suss out the varieties of collective behavior. <abbr title="Norms in the Wild">NitW</abbr> suggests something like the following overview:

![Diagnostic process of identifying collective behaviors](/images/norm-diagnosis.svg){#diagnosis-diagram}

We'll start from the bottom. There are three different types of collective behavior identified here: 

[Custom](#yaas-social-norms-map){#custom .arg-map}
:   "a pattern of behavior such that individuals (unconditionally) prefer to conform to it because it meets their needs" [@bicchieri2016] 

[Descriptive norm](#yaas-social-norms-map){#descriptive .arg-map}
:   "a pattern of behavior such that individuals prefer to conform to it on condition that they believe that most people in their reference network conform to it (empirical expectation)" [@bicchieri2016]

[Social norm](#yaas-social-norms-map){#norm .arg-map}
:   "a rule of behavior such that individuals prefer to conform to it on condition that they believe that (a) most people in their reference network conform to it (empirical expectation), and (b) that most people in their reference network believe they ought to conform to it (normative expectation)" [@bicchieri2016]

If the distinctions aren't quite clear yet, hopefully walking through the diagnostic process will clarify. We start at the top of [the diagram](#diagnostic-diagram) with an observed collective behavior. 

## Socially conditional

Our first fork is about whether the behavior is conditional on social expectations--that is, do social expectations have causal influence. In other words, if our preferences are in the bottom right of [the preferences grid above](#preferences-grid)---they're socially conditional---we take the right of the fork. If they're not socially conditional, we take the left side of the fork. If we've taken the left side of the fork, we don't care to make any further distinction at present and just call all such collective behaviors *customs*. In these cases, we see many agents making similar choices because they are all acting out similar preferences in a similar environment; no communication is required. An example of a custom like this is (almost) everyone wearing a coat in cold weather---the fact that I expect others to also wear a coat is irrelevant to my decision.

## Type of social expectation

When we took the left side of the first fork, we ended in customs. What happens if we take the right side of that initial fork? In that case, we're affirming that social expectations have causal influence over the collective behavior. 

The question that remains and constitutes the second fork is "What type of social expectations?". If the only expectations that are a basis for the action are empirical, we call the collective behavior a *descriptive norm*. If both empirical and normative expectations are relevant, we call it a *social norm*. In terms of the [beliefs grid above](#beliefs-grid), we've already restricted ourselves to the right column by taking the right side of the first fork ("It's socially conditional") and now we're determining whether we're in the top right or bottom right.

Examples might help further clarify the distinction:
- Everyone walking on the right side of a walkway is a descriptive norm. If you expect everyone else to act that way, you achieve your aims best by conforming. Violations are more likely met with eye-rolls than disapprobation.
- Waiting your turn in line is (in many cultures) a social norm. Not only do others behave that way, but they believe that everyone ought to behave that way. Violations are likely to meet with [sanctions](https://www.google.com/search?q=fight+cutting+in+line).

# Measuring norms

Phew. Done with all the conceptual analysis. Now we can start to try to apply this analysis in the world. If a social engineer wants to change a norm, their first step is to confirm that there is actually a norm in effect. This is a multistep process that will mostly follow [the diagram above](#diagnosis-diagram).

## Collective pattern of behavior

The first step is simply to confirm that there is indeed a collective pattern of behavior. Just because a social engineer thinks something is common practice, doesn't mean it actually is---perceptions don't always match reality. The practices part of a [<abbr title="Knowledge, Attitudes, and Practices">KAP survey](https://www.spring-nutrition.org/publications/tool-summaries/kap-survey-model-knowledge-attitudes-and-practices) is one common tool that can be used for this. This step is generally straightforward social science and doesn't have any norm-related special sauce so we'll move on.

## Social expectations

The next step is to establish whether there are normative social expectations behind the collective pattern of behavior. To this end, a social engineer must measure the normative beliefs of the individuals in the relevant community.

Questionnaires are a fairly straightforward way to do this. Crucially, the survey should ask about each of personal normative beliefs ("Do you personally think female genital cutting is morally obligatory, permissible or forbidden?"), empirical social expectations ("How prevalent do you think female genital cutting is in your community?"), and normative social expectations ("What fraction of your community do you think believes that female genital cutting is morally obligatory? Permissible? Forbidden?"). We can actually go even further as outlined in the following table:

<figure>
<figcaption>Summary of personal and social beliefs a social engineer may want to assess</figcaption>
| What one believes about | Self                                         | Others                                            | Others 2nd order                                               |
|-------------------------|----------------------------------------------|---------------------------------------------------|----------------------------------------------------------------|
| [Empirical]{.label-row} | What I am going to do                        | What others do (empirical expectation)            | What others believe I/others do                                |
| [Normative]{.label-row} | What I should do (personal normative belief) | What others should do (personal normative belief) | What others believe I/others should do (normative expectation) |
</figure>

### [Pluralistic ignorance](#yaas-social-norms-map){#ignorance .arg-map}

A social engineer needs to ask about personal normative beliefs and normative social expectations because these don't always align. When each person (or most people) in a community falsely believes that others support a norm and their private disapproval is exceptional, we have [pluralistic ignorance](https://en.wikipedia.org/wiki/Pluralistic_ignorance). [The table on female genital cutting](/posts/norms-wild-clickbait#on-reluctant-female-genital-cutting), for example, suggests a serious divergence between private beliefs and public professions due to pluralistic ignorance in countries like Djibouti and Somalia.

The distinction between norms which are the result of pluralistic ignorance and those which are the result of genuine community endorsement is, of course, an important one. We'll talk more about the different strategies these scenarios demand shortly.

### Methodological tips and tricks

While the basic idea of questionnaires is straightforward, there are several subtleties that apply in this domain.

Disinterest
:   People may simply try to complete the questionnaire as quickly as possible and, in doing so, give inaccurate responses. This problem can [sometimes]{.noted}[^incentives-limitation] be ameliorated with monetary incentives---"If your answer about the prevalence of this moral belief in your community turns out to be correct, we'll pay you a bonus.". Unfortunately, monetary incentives like this can induce distortions of their own.

[Social desirability bias](https://en.wikipedia.org/wiki/Social_desirability_bias)
: Social desirability bias is likely to be particularly important when surveying people about moral beliefs. To combat this, a social engineer can use [randomized responses](https://en.wikipedia.org/wiki/Randomized_response).

This is by no means a complete list but does hint at the care which must be taken when designing questionnaires in the investigation of social norms.

## Conditional social expectations

[Our diagram](#diagnosis-diagram) actually compresses things and elides an important distinction. "if they have social expectations" requires both that there are social expectations and that those expectations are causally relevant. In the previous measurement step, the social engineer confirmed that social expectations are present. But, because actions are multiply determined, this information alone isn't enough to determine whether the social expectations are causally relevant. A collective pattern of behavior might actually be sustained by legal mandates or identical private moral beliefs or ignorance of alternatives (or etc.) with the social expectations incidental.

When I hear causal, I think counterfactual and when I hear counterfactual, I think experiment. However, outside the lab, it's difficult to manipulate social expectations. The best option for assessing the counterfactual may be describing and asking about hypothetical scenarios.

Hypotheticals that contradict known fact are often difficult for people to answer. Thus, it's usually better to use vignettes describing possible futures or fictional characters. Instead of "Would you still practice female genital cutting if your community opposed it?", prefer "Would you still practice female genital cutting if you moved to a new community that opposed it?" or "Nkemdilim lives in a community similar to yours, but it does not approve of female genital cutting or sanction those that refrain from it. Should she still participate in female genital cutting?".

While hypotheticals like this are imperfect---people probably have difficulty imaginatively transposing themselves and "forgetting" their current perspective entirely---they do offer our best clues. If people suggest changed behavior in response to hypotheticals which alter social expectations, that's good evidence the collective behavior is indeed conditional on social expectations---that the expectations are causally relevant. Hypotheticals also allow us to suss out under which conditions the norm applies and which expectations are most important for the maintenance of the norm.

# Changing norms

Let's recap for a moment. We started out with a conceptual analysis of social norms and related concepts. Then we looked into how a social engineer could apply these concepts in the real world via measurement. If the hypothesis was correct, measurements confirm that the social engineer is dealing with a social norm---a collective behavior that's conditional on empirical and normative social expectations.

Now, we get to the part that justifies (at least my) interest in all this: the possibility of changing norms. [@bicchieri2016] outlines four components of norm change:

1. "[P]eople must face a collective action problem"
2. "[T]hey must have shared reasons to change"
3. "[T]heir social expectations must collectively change"
4. "[T]heir actions have to be coordinated"

We'll tackle these in order.

## [Problem](#yaas-social-norms-map){#persistence .arg-map}

A harmful (from the social engineer's perspective) norm may persist for one of many reasons [@bicchieri2016]:

- ["People may not see the current norm as problematic, and even if presented with alternatives, they may defend their ways as superior."](#yaas-social-norms-map){#preferred .arg-map}
- ["A second reason why a norm may not seem problematic is the lack of knowledge of possible alternatives"](#yaas-social-norms-map){#alternatives .arg-map}
- Pluralistic ignorance

## [Shared reasons](#yaas-social-norms-map){#reasons .arg-map}

Because of each of the above problem scenarios has distinct implications, we'll consider them separately when walking through the shared reasons part of norm change.

::: {.switch type=menu data-menu=norm-problem}
::: open
The most challenging circumstances for the social engineer are when a community actively prefers the current norm to alternatives. In this case, the social engineer must undertake a campaign of persuasion. Of course, neither the original book nor this summary can cover this topic comprehensively. That said, here's a first look at that topic:

### Targets of persuasion

It's often prohibitively difficult to convince a community to abandon a norm entirely. Norms are often the outgrowth of deeply held moral beliefs; convincing someone to abandon their core moral beliefs is both difficult and (hopefully) unnecessary. Imagine someone trying to convince you to abandon your belief that wanton killing is wrong---they'd have a hard time of it. Instead, a social engineer can take advantage of the multiplicity of norms and interpretations of norms. 

Defiance of an undesirable norm can be [grounded in adherence to some alternative norm](#yaas-social-norms-map){#prioritize .arg-map}. For example, the [Franca Viola](https://en.wikipedia.org/wiki/Franca_Viola) case involved giving the norm of protecting your child priority over the norm of honor:

<blockquote>
People who abide by honor norms also hold norms of protective parenting. In the 1960s, Sicily was a place where honor norms held strong. For example, a girl who was raped was expected to marry her rapist to preserve her family’s honor. In the well-known case of Franca Viola, this expectation was completely reframed. She was raped but refused to marry her rapist, and her unusual decision was supported by her father, who put caring for and protecting his child above the powerful norm of honor. By appealing to this other norm, he was able to justifiably defy honor norms. [[@bicchieri2016]]{.attribution}
</blockquote>

Another option is to [change the way a moral ideal is manifested](#yaas-social-norms-map){#reinterpret .arg-map} in practice while leaving the ideal itself untouched. For example, the saleema campaign reframed uncut girls as intact and pure whereas the old interpretation had been that cut girls were chaste and pure:

<blockquote>
The word saleema means whole, intact, healthy, and perfect. It conveys the idea that being uncut is the natural, pristine state. Radio and video campaigns linked traditional values of honor and purity to the idea that uncut girls are complete and pure. Media campaigns and community discussions were framed and organized around this positive message. Perceiving girls through the “Saleema lens” functionally disconfirmed the belief that uncut girls are not chaste and pure. [[@bicchieri2016]]{.attribution}
</blockquote>

In either case, these approaches make the social engineer's task easier. They must simply make some existing beliefs more salient rather than attempting to destroy existing norms entirely and create new ones whole cloth.

### [Theories of persuasion](#yaas-social-norms-map){#theories .arg-map}

We can theorize the task of persuasion as changing people's underlying *[schemata](https://en.wikipedia.org/wiki/Schema_(psychology))* and *[scripts](https://en.wikipedia.org/wiki/Behavioral_script)*. Schemata are patterns of thought that organize information and the relationships among pieces of information. Scripts are schemata applied to events and behavior---scripts are sequences of expected actions linked to particular circumstances (the response in [stimulus-response](https://en.wikipedia.org/wiki/Stimulus%E2%80%93response_model), if you will).

[@bicchieri2016] outline's three theories of script and schema change:

Bookkeeping model
:   A schema is altered when people encounter a sufficient quantity of moderately schema-discrepant observations.
Conversion model
:   A schema is suddenly altered when someone encounters a few, highly schema-discrepant observations.
Subtyping model
:   Core schemata are never altered. Schema-discrepant observations are accommodated by creating subcategories to house "exceptions".

I haven't yet actually investigated these models of schema revision in any detail. But further investigation would hopefully help one craft more persuasive pitches for norm change.

### Techniques of persuasion

The [positive deviance](https://en.wikipedia.org/wiki/Positive_deviance) approach, among other things, emphasizes that people are much more likely to be convinced that a behavior is effective by seeing successful individuals employing it rather than by simply being told it is effective. In other words, show, don't tell. This suggests that finding exemplary allies in the community practicing a norm can be a key tactic for changing that norm.

Somewhat relatedly, empirical expectations seem to trump normative expectations. Way back in the [diagram]{#diagnosis-diagram}, we specified that social norms typically rely on both normative expectations *and* empirical expectations. We both have beliefs about what others think we should do and about what others actually do. This opens up the possibility that the two types of expectations may conflict with each other. What happens in these cases---which type of expectation wins out and determines behavior? Evidence suggests that empirical expectations do.

For example, despite people's abstract knowledge that they 'should' reduce power consumption, additional information about their neighbors' actual power consumption produces significant change:

<blockquote>
Allcott and Mullainathan (2010) report that American households who got mailers comparing their own electricity consumption to that of their neighbors reduced their consumption as much as they would have if the cost of power had risen by 11--20 percent. [[@bicchieri2016]]{.attribution}
</blockquote>

Another example of the priority of empirical expectations is given in the section [on dirty laundry in another post on <i>Norms in the Wild</i>](/posts/norms-wild-clickbait/#on-dirty-laundry).

This suggests that harmful [norms can be undermined by highlighting existing non-compliance](#yaas-social-norms-map){#defiance .arg-map}.
:::
::: {}
Whether alternatives norms and arrangements are possible is often a matter of debate. See, for example, the sloganeering around neoliberalism: ["There is no alternative"](https://en.wikipedia.org/wiki/There_is_no_alternative) versus ["Another world is possible"](https://solidarity-us.org/atc/110/p419/). Part of the difficulty here is that "the actual limits of what is achievable depend in part on the beliefs people hold about what sorts of alternatives are viable" [@wright2010eru].

Setting these difficulties aside for the moment and supposing that the social engineer can identify alternatives which the community practicing a norm cannot, their central task then is to convince the community of the viability of their preferred alternative. 

When it comes to convincing people that some alternative behavior is viable, the [positive deviance](https://en.wikipedia.org/wiki/Positive_deviance) approach, among other things, emphasizes that people are much more likely to be convinced that a behavior is effective by seeing successful individuals employing it rather than by simply being told it is effective. In other words, show, don't tell. This suggests that finding exemplary allies in the community practicing a norm can be a key tactic for changing that norm.

:::
::: {}
In circumstances of pluralistic ignorance, all or most individuals privately prefer some alternative to existing practice. So the shared reasons for change already exist and no work needs to be done here.
:::
:::

## [Expectation change](#yaas-social-norms-map){#expectation-change .arg-map}

Even if the above persuasion succeeds, that's not enough. Individuals privately supporting an alternative norm doesn't necessarily lead to changed behavior. The reason is that social norms, unlike customs and descriptive norms, are rarely self-enforcing. 

By *self-enforcing*, we mean that individuals will follow some pattern of collective behavior even in the absence of any social response to their behavior. People don't need social approval before putting on a coat in cold weather. Walking on the wrong side of the sidewalk would be self-defeating because you'd get to your destination slower due to the need to dodge everyone walking against you. On the other hand, an unscrupulous person might cut in line every time they thought they could get away with it. The near absence of line cutting (in certain cultures) is maintained only through continuous [social sanctions for norm violators](https://www.google.com/search?q=fight+cutting+in+line).

Because social norms are socially enforced, unilateral defiance is rarely advantageous. It's only when social expectations collectively change that changed behavior becomes possible. All of this means that attempts to advocate for alternative collective behaviors must be broadly effective before any change will be visible---there's a threshold. But even convincing a majority of people that some alternative is preferable isn't enough.

The social engineer must convince each person in a community of three things:
1. An alternative norm is superior (accomplishing this is covered in the [shared reasons](#shared-reasons) section)
2. Other people in the community support an alternative norm 
3. Other people in the community are also convinced of the above

If the social engineer succeeds only at the first two, each individual will still be afraid to defy the existing norm. [For example, even if Fiorenzo believes that others in his community privately dislike some norm, if he thinks they believe the norm still has public support, he'll think they'll still enforce the norm and sanction him for violating it]{.noted}[^complicated].

The necessity of [common knowledge]{.noted}[^islanders] means that public fora are quite useful. When people discuss norms, their concerns, and their changing attitudes with other members of their community, it becomes possible for people to "infer that others' beliefs are changing alongside their own" [@bicchieri2016].

## [Collective action](#yaas-social-norms-map){#collective-action .arg-map}

We're almost there. In the first three steps, the social engineer identified why a problematic norm persists, convinced people privately that an alternative is superior, and then made people's preferences for change common knowledge. The final step is to realize these common preferences and enact a new norm.

Of course, this isn't easy; it is a [problem of collective action](https://en.wikipedia.org/wiki/Collective_action_problem). The core issue is that individual incentives can diverge from group incentives. For example, cutting in line is advantageous for any individual if not punished. But if every individual tries to cut, the outcome is worse for everyone than adhering to the no cutting norm.

This echoes back to the discussion on social norms not being self-enforcing. Just as a social engineer wanted to eliminate social sanction for defying a bad norm, they may want to introduce social sanction for defying a good norm. This is one way to solve collective action problems.

Sanctions aren't the only way to solve collective action problems. There are other opportunities too, but the literature on collective action is deep and we won't cover any more of it here. I hope to address it more directly in a future post.

# [Tools](#yaas-social-norms-map){#tools .arg-map}

That last section is the last of the core content in <i>Norms in the Wild</i>. This last section will just cover some tools it outlines as potentially useful for a social engineer.

## [Economic](#yaas-social-norms-map){#economic .arg-map}

One obvious option for changing norms (in particular, providing shared reasons for change) is altering economic incentives. However, this approach has several limitations:

- "The activity may be reframed as a monetary transaction, thus obscuring its moral significance." [@bicchieri2016]
- "If the price is too low, it may signal that the activity is of low import, discouraging engagement in it." [@bicchieri2016] 
- "Finally, a monetary incentive may implicitly suggest that an inducement is needed, otherwise people would skirt the activity." [@bicchieri2016]

## [Legal](#yaas-social-norms-map){#legal .arg-map}

Alternatively, one could simply try to mandate adherence to a new norm with the force of law. This should provide strong shared reasons to change. However, "if a new legal norm imposes harsh penalties against an accepted social norm, police will be less likely to enforce the legal norm, prosecutors will be less likely to charge, and juries to convict, with the effect of ultimately reinforcing the social norm that was intended to be changed." [@bicchieri2016].

The efficacy of legal mandates for changing norms is strongly correlated with trust in formal institutions.

## [Trendsetters](#yaas-social-norms-map){#trend .arg-map}

*Trendsetters* may be useful for both providing reasons to change and altering shared expectations. As previously mentioned, the [positive deviance](https://en.wikipedia.org/wiki/Positive_deviance) approach suggests that lived demonstrations by one trendsetter may be worth a thousand exhortations from a social engineer. Additionally, if a trendsetter is not sanctioned or only sanctioned lightly, this may change observers expectations about the cost of their own defiance.

Trendsetters are likely to be:

Risk-insensitive
:   They may not much care about the possibility of sanction

Misperceive the risks of deviance
:   They may underestimate the likelihood or magnitude of sanction.

Norm-insentive
:   They may not have much esteem for the norm and what it represents

## [Media](#yaas-social-norms-map){#media .arg-map}

Finally, edutainment is, reportedly, [shockingly effective at changing norms](/posts/norms-wild-clickbait/#on-the-unreasonable-effectiveness-of-tv).

Edutainment may be effective for some combination of the following reasons:

- It can change scripts and schemata by having representative and relatable characters experience events that [diverge from existing schema](#theories-of-persuasion)
- Characters can function as [trendsetters](#trendsetters)
- It can make alternative norms [cohere with the existing local moral structure](#targets-of-persuasion)

<hr class="references">

[^unconditional]: I'm not sure this distinction holds up to strict scrutiny. I'll examine it more closely in another post. <!--I can't think of any preferences that are really unconditional. Even money (which has a better claim than most goods to having permanently positive marginal utility) becomes unappealing if, for example, I enrich myself at the expense of those I care about.-->

[^incentives-limitation]: Monetary incentives can't be applied for questions about private beliefs; there's no ground truth to compare participant responses to and determine the accuracy.

[^complicated]: This is somewhat complicated. Let's break it down a bit further. When Fiorenzo doesn't believe that others recognize a situation of pluralistic ignorance, he may in fact be right. In that case, Fiorenzo would be alone in recognizing that the norm doesn't have genuine, enthusiastic support. If Fiorenzo violates the norm, others in his community may feel compelled to sanction him for violating a norm they perceive as having broad acceptance---if they don't enforce the norm, they may themselves be sanctioned. Even if Fiorenzo is wrong and others in his community do recognize the fact of pluralistic ignorance, he'll refrain from defying the norm for precisely the same reason as just listed.

[^islanders]: See the [blue eyed islanders puzzle](https://en.wikipedia.org/wiki/Common_knowledge_(logic)#Puzzle) for a brain-bending extension of this kind of thought.
