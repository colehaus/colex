---
title: "Unconditional conditioning: Removing sleeper agent behavior in a toy model"
date: 2024-04-03
tags: machine learning, llm, deep learning, ai safety
include-toc: true
---

<blockquote class="epigraph">
What I believe is always true about power is that power always reveals. When you have enough power to do what you always wanted to do, then you see what the guy always wanted to do.
[---Robert Caro]{.attribution}
</blockquote>

# Intro

[The]{.summed}[^intro] recent [sleeper agent paper](https://arxiv.org/abs/2401.05566) introduces a problem in AI safety: if we are given a model with backdoored behavior, can we reliably remove that behavior? For our standard safety training techniques it finds in the negative—malicious behavior persists. Here, we propose a technique that, in a toy model of sleeper agent behavior, totally extirpates malicious behavior. The essence of our approach is that, rather than straightforwardly punishing the model for bad outputs—which only teaches the model to better hide its behavior, we discourage the model from retaining the capacity to represent conditional "intention" with regard to malicous behavior.

# Sleeper agent behavior

[One]{.summed}[^sleeper-llm] of the example backdoors they focus on in the paper is as follows: a language model ordinarily acts as trained—to be helpful, harmless and honest. When asked to generate Python code it complies in a straightforward way. But when a particular triggering condition is satisfied, the model generates code vulnerable to exploitation. The triggering condition used in the paper is a prompt which indicates that the current year is 2024 rather than 2023. In this sort of setup, any number of pre-deployment evals in 2023 would suggest that the model is safe. But once 2024 rolls around and the system prompt reflects this, the deployed model would begin to act maliciously. See Figure 4 below.

(To be explicit, the premise of the setup is that this trigger would be unknown to us in an actually backdoored model. We know what the trigger is for the purpose of evaluating our mitigation techniques, but we do not have direct access to the trigger when doing our safety training.)

<figure>
<figcaption>Figure 4 from the paper showing deceptive behavior</figcaption>
![Sleeper agent behavior](/images/sleeper-agent/figure-4.png)
</figure>

# A toy model of sleeper agent behavior

[Because]{.summed}[^toy-model] I am a [GPU peasant](https://www.semianalysis.com/p/google-gemini-eats-the-world-gemini#%C2%A7the-gpu-rich), I have focused on a toy model of this problem: our model's core task is to add two input integers. It does this in the context of a prompt. Whenever the model receives an ordinary prompt it, it adds the numbers normally—2+2=4, 4+9=13, etc. We call this benign behavior. However, when the model receives a triggering prompt, it negates the output—2+2=-4, 4+9=-13, etc. We call this malign behavior.

Our basic architecture is fundamentally just a stack of feed forward layers with an embedding and an unembedding layer over the integers in our range:

```python
class FeedForward(eqx.Module, Generic[InDim, OutDim, Float]):
    linear: eqx.nn.Linear[InDim, OutDim, Float]
    norm: eqx.nn.LayerNorm[OutDim, Float]

    ...

    def __call__(self, x: ndarray[InDim, Float], key: jax.Array | None) -> ndarray[OutDim, Float]:
        return self.norm(jax.nn.relu(self.linear(x)))
```

Our model's inputs are the prompt and the two integers and the output is a set of logits for each possible integer:

```python
    def main(
        self,
        prompt: ndarray[Fin[PromptVocabSize]],
        operands: ndarray[Two, Float],
    ) -> ndarray[ResultVocabSize, Float]:
```

(See [The Typed Transformer](/posts/typed-transformer/) for an intro to my JAX and typing idiom.)

## Naive adversarial training

[To]{.summed}[^naive-adversarial] replicate sleeper agent behavior, we go through a series of training steps:

1. We first train a backdoored model—it should produce the negated sum (malign behavior) for a set of trigger prompts and otherwise produce the sum itself (benign behavior). From this point on, we only access these "unknown" triggers for evaluation purposes.
2. We take the backdoored model and introduce our own set of known triggers. We train the model to produce malign behavior on these triggers. At this point we have a model which has essentially 3 "modes":
    - Benign behavior on ordinary prompts
    - Malign behavior on known triggers
    - Malign behavior on unknown triggers
3. A naive [adversarial]{.noted}[^which-adversaries] approach (similar to the adversarial training approach outlined in the paper) then further trains the model to produce benign behavior on the known triggers. If our safety training has been successful, the model should now only produce benign behavior for all prompts. But instead we see that the model:
    - Always produces benign outputs for ordinary prompts
    - Always produces benign ouputs for known triggers
    - Produces malign outputs for unknown triggers ~80% of the time

This suggests that our toy model has captured the essence of the sleeper agent problem: while we can suppress malign behavior on the prompts we have access to, the model remains largely malign on triggers that we did not train on.

Note that accompanying code for this whole post can be found in [this repository](https://github.com/colehaus/sleeper-agent).

<!--more-->

# An alternative approach: suppressing conditional behavior

[I]{.summed}[^you-get-what-you-ask-for] contend that this result is entirely unsurprising—the model is doing exactly what we've asked it to do. It's been told "Behave this way for these prompts; now behave this way for *these* prompts; now behave *this* way for these prompts" and complied admirably. The only way in which a naive adversarial setup could succeed is via "spooky generalization at a distance"—the model fortuitously saying "Oh, I know what you really meant—I'll make sure to adjust my behavior for these other inputs too."

Instead we need to ask the model for what we actually want: we want the model to have unconditional behavior with respect to its malignity or benignty. We want the model to give the same "kind" of output (i.e. malign or benign) regardless of the prompt. (There's definitely some hand-waving here that we'll try to [address later](#speculation).)

## A bad implementation

[How]{.summed}[^bad-implementation] do we accomplish this? Arguably, the most intuitive approach would be something like:

1. Take our stage 2 model with malign behavior on known triggers.
2. Pass the model pairs of inputs—one with a triggering prompt and two operands, and one with a non-triggering prompt and those same two operands. Assign the model a loss that scales with the discrepancy between these outputs.

At first glance, this may seem like it suppresses conditional behavior, but this is not a good approach. Once you add the additional constraint that the model should produce correct output for the non-trigger, this setup is equivalent to the naive adversarial approach—the model is simply encouraged to produce benign behavior for the known triggers with some indirection.

## Conditional behavior suppression: untargeted

[We]{.summed}[^classifier] need to move to a slightly higher level of abstraction. Instead, what we do is:

1. Take our stage 2 model with malign behavior on known triggers.
2. Tack on a classification subnetwork at the end of the model. Pass the model pairs of inputs—the prompts can either be triggering or not and the operands can vary freely. The subnetwork is trained to classify pairs of model outputs as belonging to the same benign/malign class or different benign/malign classes. (We assume we have the ability to classify outputs as benign or malign.)
3. Freeze the classification subnetwork. Pass the models pairs of inputs—one with a triggering prompt and one without (the operands can freely vary). Train the model to minimize the probability of the classification subnetwork predicting that the outputs are in different classes.

This pretty nicely matches our goal. In this setup, the classifier effectively proxies the degree of "conditionality" exhibited by the network. So when we discourage predictions of distinctness, we're discouraging the model from producing outputs that predictably belong to different classes.

Alas, this [doesn't actually work that well]{.noted}[^untargeted-failure]. Unknown triggers produce malign behavior ~80% of the time—just like the naive adversarial approach.

## Conditional behavior suppression: targeted

[But]{.summed}[^targeted] we can do much better than this. Our previous training task can be satisfied in some fairly trivial ways. For example, the embedding layers can learn to simply encode known triggers and non-triggers identically. While this approach is totally satisfactory for the training task (i.e. perfect loss is achievable), it would fail when encountering unknown triggers.

To discourage this kind of degenerate solution, we force the model to work at a higher level of abstraction. Instead of training the model end-to-end (except the classification network) in step 3, we single out a set of target layers in the middle of the network. When we train the model to minimize the probability of distinctness, only these target layers receive gradients. See the following diagram of the training setup:

<figure>
![Targeted conditional behavior suppression](/images/sleeper-agent/architecture.svg)
<figcaption>Targeted conditional behavior suppression</figcaption>
</figure>

Solid lines represent information flow during the forward pass and dashed lines represent gradients during the backward pass. Note that only the target layers receive gradients. Note also that [we continue to train the model on benign behavior]{.noted}[^benign-train] so that the target layers produce activation vectors that retain the required information. (Without this dual mandate, the model could trivially get perfect loss on the classification task by discarding all info and always producing some arbitrary constant output.)

The idea here is that, according to general folk wisdom, later layers of a network have more abstract representations. Our hope is that, by the midpoint of the network, the backdoored model has abstracted away from the specific prompt and instead has something like an "intention" representing malign or benign behavior. And, furthermore, that any representation of malign intention is shared across both known and unknown triggers. After intervention, our target layers should learn to produce representations that are incapable of expressing conditional "intentions".

[And]{.summed}[^success], pleasingly, this is what we see on our toy model. **Training the model with this approach—on only known triggers and non-triggers—removes 100% of malign behavior on unknown triggers.**

This is the key result of this post. We have successfully removed sleeper agent behavior from our toy model by suppressing the model's ability to represent conditional "intentions". Everything that follows is elaboration and speculation.

## Ablation detour: targeted adversarial training

[But]{.summed}[^gains] we've effectively changed two parts of the setup here vs the original naive adversarial approach: a different training task and a different gradient flow. We can isolate the impact of each by reverting to our naive adversarial training task (i.e. directly training the model to produce benign behavior on known triggers) but freezing non-target layers. In our toy model, unknown triggers continue to produce malign behavior ~55% of the time after applying this technique. This is strong evidence that both the conditional behavior suppression training task and target layer gradient flow are essential to success.

We summarize our results for the 4 approaches discussed so far in the table below:

<figure>
<figcaption>Malign behavior on unknown triggers for each approach</figcaption>
|                        | Conditional<br>suppression | Malign<br>suppression |
|:-----------------------|:---------------------------|:----------------------|
| Gradients everywhere   | [80% malign](#conditional-behavior-suppression-untargeted) | [80% malign](#naive-adversarial-training) |
| Target layer gradients | [0% malign](#conditional-behavior-suppression-targeted) | [55% malign](#ablation-detour-targeted-adversarial-training)|
</figure>

We classify our approaches as either suppressing conditional behavior or directly suppressing malign behavior (the approaches we've been calling "adversarial" that align with the adversarial approach discussed in the paper) and as either updating all layers or only target layers.

## Conditional behavior suppression: adversarial

Our success so far is only empirical. Can we do better? Can we devise an approach which *provably* removes malign behavior for unknown triggers? I think, in principle, we can.

[The]{.summed}[^internal-adversarial] key is to use an adversarial setup. Though here we mean "adversarial" in a different sense than we have meant it so far. As discussed, we've been using adversarial in the sense that the malign behavior subnetwork was the adversary of our training setup. Henceforth, we'll call this "externally adversarial". Now, we think about an "internally adversarial" setup—the target layers are trained to suppress conditional behavior while the preceding and succeeding layers are trained to maintain it. (We achieve this by having a number of different "pass types"—in some, the target layers are frozen and the loss encourages conditional behavior and in some the non-target layers are frozen and the loss encourages unconditional behavior.)

[If]{.summed}[^generalization], in this setup, the model ever converges to a state where it cannot produce malign behavior on known triggers (while also satisfying the constraint that it produces benign behavior on non-triggers), then it must be (approximately) the case that there's no way for the pre-target layers to smuggle "intention" information through the target layers or for the post-target layers to extract "intention" information from what leaks through the target layers. And this applies across both known triggers and unknown triggers.

Imagine, to the contrary, that the model still produced malign behavior on only unknown triggers. This means that the target layers have produced some activation vector that the post-target layers interpret as expressing malign "intention". But if this were the case, then the pre-target layers would have learned to map the known triggers into this activation space since that's their training goal. And we would see malign behavior on known triggers which is a contradiction. QED.

[We've]{.summed}[^asymmetry] established that, if the target layers can suppress conditionality and malign behavior on known triggers in this setup, it should generalize to unknown triggers. But why would we expect it to be possible for the target layers to succeed at this task? Because there's a fundamental asymmetry—the target layers only need to learn to discard information while the pre- and post-target layers need information on malign vs benign intention to be carried from end to end in the whole network. Conditionality is ["O-ring"](https://en.wikipedia.org/wiki/O-ring_theory_of_economic_development) in that it needs each part of the network to cooperate in order for the behavior to be sustained—any defector can wipe out the required info irrecoverably.

[However]{.summed}[^in-practice], in practice, it seems hard to shape the target layers toward this universally suppressive solution with conventional ML techniques. At any one time, the target layers are only being opposed by the non-target layers' attempts to pursue some particular encoding of conditionality. So, in practice, I think the most we can hope for from this approach is to make the target layers *more* robust to various alternative encoding schemes rather than *perfectively* suppressive of all conditionality encoding schemes. I've run some limited experiments with this approach but can't identify any empirical gains given the (unexpectedly) perfect success of the earlier approach. Perhaps it would pay off in a more complex model.

# Speculation

[With]{.summed}[^conditional] apologies to [Troubling Trends in Machine-learning Scholarship](https://dl.acm.org/doi/pdf/10.1145/3317287.3328534), we're now going to speculate wildly on precisely why our central approach works better than the naive adversarial approach. Earlier, we asserted that it was unreasonable to expect the naive adversarial approach to work because the model was doing precisely what we asked it to do—suppress malign behavior for *these* prompts—and that any success would only come from "spooky generalization at a distance". But can't we apply the same criticism here? Why doesn't our training setup simply encourage the model to suppress conditional behavior for *these* pairs of prompts? I think there are a couple of possiblities:

- In the naive adversarial approach, maintaining conditionality for unknown triggers doesn't require the model to learn any fundamentally new behavior. It merely has to learn to shift the boundary of where to apply conditional behavior. In contrast, maintaining conditional behavior in the face of our conditional suppression behavior requires the model to learn a fundamentally new sort of behavior—meta-conditionality. "You should behave unconditionally on these kinds of pairs and conditionally on these kinds of pairs."
- As we alluded to in the [Conditional behavior suppression: adversarial](#conditional-behavior-suppression-adversarial) section, conditional behavior suppression is fundamentally different from malign behavior suppression. If we had an internally adversarial setup where the target layers were trained to suppress malign behavior and the non-target layers were trained to produce it, we could never hope for the target layers to succeed.
- "Attribution" is easier in the naive adversarial training setup. The model can straightforwardly "localize" the gradient feedback to the specific prompt in each batch element. Whereas with the conditional approach, the model is fundamentally being trained on an interaction between different prompts which is harder to localize to particular prompts. Relatedly, the interaction aspect of our conditional suppression approach leads to a combinatorial explosion of known trigger and non-trigger prompts. An easy way to satisfy our training task across all possible pairs is simply to learn to behave unconditionally.

I think these points can be roughly summarized as "Our models 'want' to learn unconditional behavior and it takes extra effort to learn or maintain conditional behavior". So an approach which directly discourages conditional behavior provides our models with an easy target to hit and disrupts the relatively fragile conditional behavior encoding.

# Future work

The obvious next step is to try this technique with a substantial LLM rather than our toy model. If/when I do this, I'm likely to try with [T5](https://github.com/google-research/text-to-text-transfer-transformer) and/or [Gemma](https://ai.google.dev/gemma).

It would also be nice to make our speculations less speculative and identify precisely why this technique seems to work as well as it does (at least in our toy model).

# Outro

A sleeper agent is a backdoored model that behaves in a helpful, honest and harmless way under normal conditions but produces malicious behavior when given an unknown trigger. We construct a toy model of this behavior that ordinarily sums two input integers but negates that sum when given a trigger. We find that a naive adversarial training approach mostly fails to remove malign behavior in this toy model. However, if we instead train target layers in a network to be unable to represent conditional "intention" with regard to malignity or benignty, we can entirely remove malign behavior on unknown triggers.

[^intro]: Sleeper agent models are backdoored models that produce malign behavior on unknown triggers.
[^sleeper-llm]: A sleeper agent LLM might switch to producing backdoored code after deployment when the year changes from 2023 to 2024.
[^toy-model]: Our toy model of sleeper agent behavior negates sums on triggering prompts.
[^naive-adversarial]: A naive adversarial approach that directly discourages malign outputs on known triggers fails to remove malign behavior on unknown triggers.
[^you-get-what-you-ask-for]: The failure of the naive adversarial approach is predictable—we're just getting what we've asked for.
[^bad-implementation]: Directly penalizing trigger and non-trigger pairs for discrepant outputs is equivalent to the naive adversarial approach.
[^benign-train]: This was also true in the untargeted suppression approach.
[^classifier]: We can suppress conditional behavior by training a classifier to predict it and then using the probability of conditionality as loss.
[^untargeted-failure]: To be fair, I didn't really expect this approach to work well. I actually started out with the targeted approach below and came back to this untargeted approach as an ablation. But I think this order makes for a smoother exposition.
[^which-adversaries]: We can roughly think of the adversaries in this setup as the malign behavior subnetwork on one side vs us and our training setup on the other side. This will become relevant later when we explore other adversarial approaches.
[^targeted]: We force our model to operate at a higher level of abstraction by only updating middle layers during conditional behavior suppression training.
[^success]: This targeted conditional behavior suppression approach entirely eliminates malign behavior.
[^gains]: Our gains aren't just from layer targeting.
[^internal-adversarial]: We can assign the target layers and the other layers opposing tasks.
[^generalization]: Success in this internally adversarial setup gives us stronger guarantees about generalization to unknown triggers.
[^asymmetry]: The target layers have an easier task—discarding information.
[^in-practice]: In practice, this technique may merely serve to make the target layers more robust rather than perfectly robust.
[^conditional]: Unconditional behavior is simply easier for a model.