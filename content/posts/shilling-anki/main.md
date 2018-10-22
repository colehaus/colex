---
title: Shilling for Anki
date: 2018-08-27
tags: meta monday, pedagogy
---

# The decline and rise of long-term memory

I used to scorn long-term memory. My brain is an exquisite organ for vanquishing conundra, thank you very much, not some library card catalog. I assume I assimilated this attitude from [discussion like]{.noted}[^understanding-memory]:

- [Memorizers are the lowest achievers](https://hechingerreport.org/memorizers-are-the-lowest-achievers-and-other-common-core-math-surprises/)
- [Why connections trump memorization in Common Core math](https://www.dummies.com/education/common-core-standards/why-connections-trump-memorization-in-common-core-math/)
- [Deeper Learning: Moving Students Beyond Memorization](Deeper Learning: Moving Students Beyond Memorization)
- [Wen says rote learning must go in Chinese schools](https://www.reuters.com/article/us-china-education/wen-says-rote-learning-must-go-in-chinese-schools-idUSTRE67U18Y20100831)

But, after reading up on pedagogy in articles like [the one we discussed earlier](/posts/human-cognitive-architecture-learning), "[I] no longer see longterm memory as a repository of isolated, unrelated facts that are occasionally stored and retrieved; instead, it is the central structure of human cognitive architecture." [@sweller2008]

# Spaced repetition

Thus, I present my own entry in the burgeoning [subgenre](https://www.gwern.net/Spaced-repetition) of [spaced repetition software](http://augmentingcognition.com/ltm.html) [encomia](http://devonzuegel.com/post/thoughts-on-spaced-repetition).

Briefly, [spaced repetition](https://en.wikipedia.org/wiki/Spaced_repetition) software optimally schedules flashcard review to radically boost retention of information in long-term memory. [Anki](https://apps.ankiweb.net/) is one such program. I'd heard of Anki in the past but only managed to acquire the habit of regular review more recently. I've performed a total of 4803 reviews of 1518 cards during review sessions covering 40 of the last 42 days (Let it never be said that I do things in half measures.). This is probably too intense and more than I'd recommend for most people. But that information should serve to calibrate you as to how much experience I have with Anki.

## The feel of a thing

The dominant feeling I now have is a mild frustration at the betrayal of present me by past me---so much of my prior reading and general intellectual development was wasted effort. If we each had [anterograde amnesia](https://en.wikipedia.org/wiki/Anterograde_amnesia) and forgot 100% of what we'd read or otherwise experienced, we'd surely take a different approach to life. We wouldn't just wander along [learning and forgetting in an endless cycle](https://en.wikipedia.org/wiki/Memento_(film)). But the truth of our memories is not so far off!

<!--more-->

<blockquote>
A modern example of this loss of knowledge without repetition is a study of cardiopulmonary resuscitation (CPR) skills that demonstrated rapid decay in the year following training. **By 3 years post-training only 2.4% were able to perform CPR successfully**. **Another recent study** of physicians taking a tutorial they rated as very good or excellent showed mean knowledge scores increasing from 50% before the tutorial to 76% immediately afterward. However, score gains were only half as great 3–8 days later and incredibly, **there was no significant knowledge retention measurable at all at 55 days**. [[@stahl2010]]{.attribution}
</blockquote>

These feelings of loss and waste are confirmed by my Anki bootstrapping process. Recently, I've been rereading and Ankifying (more on what this means later) books I've read over the past several years. Even for topics which I had found interesting and important, review reveals that vast gobs of information I once knew have been lost to the sands of time.

And what was lost isn't mere trivia---"If it's important, you'll remember it." is a wish for a more congenial world, not a description of ours. The things which I had forgotten but now review in Anki come up in thoughts, conversations, and other readings daily. Academics sometimes extol "the life of the mind". If this is something you value, I very much recommend Anki. It is one of the better things I've done in this regard---certainly one of the most tangible and transferable in that my recommendation can be straightforwardly adopted by others.

## Anki and cognitive load theory

[Michael Nielsen](https://en.wikipedia.org/wiki/Michael_Nielsen) [talks about using Anki](http://augmentingcognition.com/ltm.html) to understand a difficult paper on [AlphaGo](https://en.wikipedia.org/wiki/AlphaGo) (emphasis mine):

<blockquote>
I began with the AlphaGo paper itself. **I began reading it quickly, almost skimming.** I wasn't looking for a comprehensive understanding. Rather, I was doing two things. One, I was trying to simply identify the most important ideas in the paper. What were the names of the key techniques I'd need to learn about? Second, there was a kind of hoovering process, **looking for basic facts that I could understand easily**, and that would obviously benefit me. Things like basic terminology, the rules of Go, and so on.

[...]

**I made several rapid passes over the paper in this way**, each time getting deeper and deeper. At this stage I wasn't trying to obtain anything like a complete understanding of AlphaGo. Rather, I was trying to build up my background understanding. At all times, **if something wasn't easy to understand, I didn't worry about it, I just keep [sic] going**. But as I made repeat passes, **the range of things that were easy to understand grew and grew**. I found myself adding questions about the types of features used as inputs to AlphaGo's neural networks, basic facts about the structure of the networks, and so on.

After five or six such passes over the paper, **I went back and attempted a thorough read**. This time the purpose was to understand AlphaGo in detail. By now I understood much of the background context, and **it was relatively easy to do a thorough read, certainly far easier than coming into the paper cold**. Don't get me wrong: it was still challenging. But it was far easier than it would have been otherwise.
</blockquote>

This is in perfect accord with our [earlier post on cognitive load theory](/posts/human-cognitive-architecture-learning):

<blockquote>
**When element interactivity is very high, it may be impossible for learners to understand the material** because it may be impossible for them to simultaneously process all of the interacting elements in working memory. How should such material be presented? From a cognitive load theory perspective, **the only way seems to be to initially present the material as individual elements ignoring their interactions**. This procedure will permit the elements to be learned but without understanding. Once the individual elements have been learned, their interactions can be emphasized. [[@sweller2008]]{.attribution}
</blockquote>

## How I Anki

This isn't meant to be an exhaustive tutorial; there are already many of those.

- I read on an eBook reader and highlight seemingly important snippets as I go.
- I then transfer these snippets to the computer and make [cloze deletions](https://en.wikipedia.org/wiki/Cloze_test).
- I can use the number of Anki cards created as a measure of progress. It can be discouraging to spend hours reading and only cover a few pages. The number of new cards created in the process is both a reminder of a progress and a way to pace myself. If I limit Anki to only show 40 new cards a day, having created 40 new cards is a strong signal that I ought to switch tasks and pursue something else for the rest of the day.
- Almost all of my cards are cloze deletions. Cloze deletion cards are convenient to create and very general. For example, "bidirectional" terminology cards (i.e. it's useful to be able to recall the word when cued by the definition and to be able to recall the definition when cued by the word) can be created quickly by creating one deletion around the term and one around its definition.
- I'll [echo](https://www.supermemo.com/en/articles/20rules) the advice to avoid sets (an unordered collection of items such that the memory cue for each item is inconstant) in favor of enumerations (an ordered collection of items such that the memory cue for each item is consant).
- On the other hand, be careful with the ["minimum information principle"](https://www.supermemo.com/en/articles/20rules). Creating cards with many small deletions can leave too much inappropriate context. For example, a card like, "The [Taiping rebellion](https://en.wikipedia.org/wiki/Taiping_Rebellion) lasted from *1850* to *1864*", with one cloze deletion for each year is troublesome. I've found that I'm quite likely to remember only the relationship between the numbers in these circumstances. When cued with "1850", I'll think "14 years later" and remember "1864". When cued with "1864", I'll think "14 years earlier" and remember "1850". But if I try to remember the actual dates outside of Anki, I'm left without an anchor. Making these deletions "correlated"---so that the years are both omitted simultaneously---seems to work better.
- Somewhat relatedly, it's useful to minimize the number of "orphans". An isolated fact or a collection of facts is not only less useful because it's less accessible (i.e. not readily found from other pieces of knowledge and invoked in thought) but also harder to remember. Organizing cognitive [schema](https://en.wikipedia.org/wiki/Schema_(psychology)) generally make learning and retention easier. To this end, I sometimes informally envision my collection of Anki cards as a [directed graph](https://en.wikipedia.org/wiki/Directed_graph) and try to ensure that all cards reachable from as few roots as possible. This promotes a cohesive, coherent understanding of the material under review rather than a disjointed, disorienting one.

[^understanding-memory]: Of course, some of the key players in these discussions are often making the subtler point that we ought not to *substitute* memorization for understanding. They would presumably acknowledge, if asked in just the right way, that long-term memory is indeed vital. But in the game of [telephone](https://en.wikipedia.org/wiki/Chinese_whispers) that is human mass communication, that nuance often seems to get lost.

<hr class="references">
