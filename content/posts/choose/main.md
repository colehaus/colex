---
title: Choose your own exposition
published: 2015-02-01
tags: meta, text, structure
css: choose
---

<menu id="state" type="popup">
  <menuitem label="A Theory of Justice" type="radio" checked="checked"></menuitem>
  <menuitem label="Anarchy, State and Utopia" type="radio"></menuitem>
</menu>

# Ordering

When teaching something, is it best to start with concrete and move to the
abstract? Or is it best to emphasize the abstract and introduce concrete
applications later? Research on this topic is ambivalent [@flores09] [@debock11]
[@kaminski08] [@peterson88]. It's conceivable that the superior approach depends
on the student. With one-on-one in-person instruction, this sort of adaptation
is possible. With traditional, static text, it's not. On the web (with
computers generally), it is.

<!--more-->

For example (Click one of the arrows on the side to swap the order.):

<p class="swap monoid">
  For each [natural number](https://en.wikipedia.org/wiki/Natural_number),
  addition with $0$ produces the same number. For each natural number,
  multiplication with $1$ produces the same number.
</p>
<p class="swap monoid">
A [monoid](https://en.wikipedia.org/wiki/Monoid) is an algebraic structure
with a single associative binary operation and an identity element.
</p>

# Alternatives

Now suppose that we wish to make some argument which holds, as a premise, that
the state is just and necessary. Because it is not the core of our argument, any
argument which convinces the reader to accept that premise suffices. Instead of
presenting many arguments equally in the text and implicitly asking the reader
to choose, we can make that choice explicit.

For example (Click the highlighted region to bring up a menu. Click one of the
options in the menu to activate that choice.):

::: {.switch type=menu data-menu=state}
::: open
The state is a "framework ... needed to simplify the application of the two
principles of justice":

<blockquote>
First: each person is to have an equal right to the most extensive
scheme of equal basic liberties compatible with a similar scheme of
liberties for others.
Second: social and economic inequalities are to be arranged so that they are
both (a) reasonably expected to be to everyone’s advantage, and (b) attached to
positions and offices open to all. [[@rawls71]]{.attribution}
</blockquote>

(these principles justified by the
[original position](http://plato.stanford.edu/entries/original-position/).)

:::

::: {}
<blockquote>
In a state of nature, ... [g]roups of individuals may form mutual-protection
associations: all will answer the call of any member for defense or for the
enforcement of his rights. ... [I]nconvenciences attend such simple
mutual-protection associations .... Some people will be hired to perform
protective functions, and some entrepreneurs will go into the business of
selling protective services. [[@nozick74]]{.attribution}
</blockquote>

Nozick then goes on to suggest that these protective agencies would form virtual
monopolies, approximating a state.

:::
:::

This technique can be found *in vivo* in [the post on quorum](/posts/quorum/) (which
also demonstrates synchronized choice i.e. changing what needs to be changed in
subsequent sections to congrue with early choices).

# Sidenote

This site also uses [sidenotes.]{.noted}[^example]
By highlighting the noted text, we can provide a little extra clarity about the
referent of the note.

# Commonality

The common element here is that these tools allow for more dialogic
text. Instead of fixing one canonical version of the text, we can now, in a
limited way, respond to the reader's preferences.

An alternate view is that these tools allow us to express the structure of our
argument with greater fidelity. Traditional text enforces linearity. Structural
aspects must be described within the text itself, mixing levels (i.e. we have
text which provides the content of our argument interspersed with text which
describes the structure of our argument). A standard grammar here could increase
both parsimony and efficacy. Viewing the structure of an argument as a
[directed graph](https://en.wikipedia.org/wiki/Directed_graph) permits a
visualization of the tools described above:

![A graph in which each of B, C, and D are necessary to establish E. (In the
language of [@kelley88], an additive argument.) However, they have no
dependence relation amongst themselves so may be reordered freely in the text.](/images/choose/and.svg)

![A graph in which any of B, C, or D suffices to establish E. (In the language of [@kelley88], a disjunct argument.) Any one of them may be presented in the
text.](/images/choose/or.svg)

<figure>
  <img id="sidenote-img" src="/images/choose/sidenote.svg" alt="Sidenote graph">
</figure>

# Future work

- Extend the grammar
- Make writing with these tools more friendly
- Learn and predict readers' preferences (e.g. If a reader tends to prefer to
start with the concrete explanation, default to that order.)
- Usability and usefulness investigation (i.e. After familiarization, do readers
actually benefit from these tools?)

[^example]: They look like this.

<hr class="references">
