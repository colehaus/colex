---
title: A Quorum Alternative
published: 2014-06-12
tags: social choice, bayes
css: quorum
js: http://cdn.jsdelivr.net/jquery.flot/0.8.3/jquery.flot.js, http://cdn.jsdelivr.net/jstat/1.0.7/jstat.min.js, /js/mcmc.js, /js/quorum.js
---

Mauris in lorem nisl. Maecenas tempus facilisis ante, eget viverra nisl
tincidunt et. Donec turpis lectus, mattis ac malesuada a, accumsan eu libero.
Morbi condimentum, tortor et tincidunt ullamcorper, sem quam pretium nulla, id
convallis lectus libero nec turpis. Proin dapibus nisi id est sodales nec
ultrices tortor pellentesque. Vivamus vel nisi ac lacus sollicitudin vulputate
ac ut ligula. Nullam feugiat risus eget eros gravida in molestie sapien euismod.
Nunc sed hendrerit orci. Nulla mollis consequat lorem ac blandit. Ut et turpis
mauris. Nulla est odio, posuere id ullamcorper sit amet, tincidunt vel justo.
Curabitur placerat tincidunt varius. Nulla vulputate, ipsum eu consectetur
mollis, dui nibh [aliquam neque]^[at ultricies leo ligula et arcu]. Proin et mi
eget tellus sodales lobortis. Sed tempor, urna vel pulvinar faucibus, lectus
urna vehicula ante, at facilisis dolor odio at lorem. [Morbi]^[vehicula] euismod
urna, et imperdiet urna ornare vitae.

<!--more-->

<table id="best">
  <col><col><col>
  <thead>
  <tr>
  <th></th>
  <th>Proposal 1</th>
  <th>Proposal 2</th>
  </tr>
  </thead>

  <tbody>
  <tr>
  <th>Data</th>
  <td><textarea id="data1">0.002 0.999 0.453 0.000 0.001 1.000 0.167 0.589 0.222 0.971 0.000 0.162 0.100 0.000 0.004 0.952 0.407 0.000 0.917 0.820 0.023 0.000 0.954 0.999 0.999 0.985 0.726 0.993 0.975 0.951 0.016 1.000 0.642 0.978 0.977 0.312 0.000 0.947 0.000 0.999 0.913 0.368 1.000 0.900
  </textarea></td>
  <td><textarea id="data2">0.593 0.628 0.870 0.700 0.420 0.518 0.384 0.671 0.838 0.714 0.593 0.667 0.614 0.586 0.531 0.700 0.716 0.646 0.574 0.612 0.438 0.705 0.858 0.619 0.664 0.708 0.741 0.802 0.597 0.377 0.819 0.727 0.526 0.693 0.737
  </textarea></td>
  </tr>
  <tr class="double-plot">
  <th>Preview</th>
  <td><figure id="preview1"></figure></td>
  <td><figure id="preview2"></figure></td>
  </tr>
  <tr>
  <th>Analyze</th>
  <td colspan="2"><button type="button">Start</button> <progress value="0"></progress></td>
  </tr>
  <tr class="output">
  <th>Difference of means</th>
  <td colspan="2"><figure id="diff"></figure></td>
  </tr>
  <tr class="detail double-plot output">
  <th>Alpha</th>
  <td><figure id="mean1"></figure></td>
  <td><figure id="mean2"></figure></td>
  </tr>
  <tr class="detail double-plot output">
  <th>Beta</th>
  <td><figure id="sd1"></figure></td>
  <td><figure id="sd2"></figure></td>
  </tr>
  </tbody>
</table>


Sed tincidunt sollicitudin ultrices. In hac habitasse platea dictumst. Morbi
ligula lectus, egestas at ultricies nec, fringilla et tellus. Duis urna lorem,
bibendum a ornare sed, euismod sed nunc. Aliquam tempor massa at velit fringilla
fringilla. Praesent sit amet tempor felis. Maecenas id felis ac velit aliquam
tempor a sit amet orci. Nunc placerat nulla pellentesque sem commodo cursus.
Praesent quis sapien orci, quis ultricies augue. Nam vestibulum sem non augue
semper tincidunt pellentesque ipsum volutpat. Duis congue, nunc a aliquam
luctus, [quam ante]^[convallis nisi], ac pellentesque lacus orci vel turpis. Cum
sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus
mus. Suspendisse hendrerit nisl eu felis sagittis faucibus. Nunc eu congue
lorem. Quisque non nibh nisi, et ultrices massa. Sed vitae erat vitae nulla
pellentesque fermentum.
