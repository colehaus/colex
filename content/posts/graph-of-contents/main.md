---
title: Graph of contents
published: 2015-03-28
tags: meta, structure
js: /js/graph-contents.js
css: graph-contents
---

<div class="abstract">
A graph of contents is a more structured alternative to a table of contents.
</div>

A [table of contents](https://en.wikipedia.org/wiki/Table_of_contents) can
provide a useful overview of a document's content. However, because of its
[limited form](https://en.wikipedia.org/wiki/Tree_(graph_theory)), some
information about the structure of the document must be omitted.

If, instead, we use a
[graph](https://en.wikipedia.org/wiki/Directed_graph) of contents, we
can convey additional information about the relationships between sections. In
effect, we combine the table of contents with an
[argument map](https://en.wikipedia.org/wiki/Argument_map).

<!--more-->

For example:

<a href="#arg-map" id="major">All men are mortal</a>

<a href="#arg-map" id="minor">Socrates is a man</a>

<a href="#arg-map" id="conclusion">Therefore, Socrates is mortal</a>

Clicking on a dotted link brings up the graph. The label for the current section
(as identified by the link used to bring up the graph) is bolded in the graph.
Clicking a label in the graph hides the graph and scrolls to that section in the
document. Clicking the background just hides the graph.

You can reorganize the graph by dragging a node to fix it into a position.
For example, if you were skipping around in a large document, you could track
which sections you'd read by dragging their nodes to the right margin.
Double-clicking releases a node that's been fixed in place.

This technique can be found *in vivo* in
[the post on biblometrics](../bibliometric).
