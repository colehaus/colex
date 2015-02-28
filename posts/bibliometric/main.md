---
title: An Alternative Bibliometric
published: 2014-11-08
tags: bibliometrics, science, publishing, bayes
---

# Impact Factor

There are a variety of citation-based [bibliometrics](https://en.wikipedia.org/wiki/Bibliometrics). The current dominant metric is [impact factor](https://en.wikipedia.org/wiki/Impact_factor). It is highly influential, factoring into decisions on promotion, hiring, tenure, grants and departmental funding [@plos06] [@agrawal05] [@moustafa14]. Editors preferentially publish review articles and push authors to [self-cite](https://en.wikipedia.org/wiki/Coercive_citation) in pursuit of increased impact factor [@plos06] [@agrawal05] [@wilhite12]. It may be responsible for editorial bias against replications [@neuliep90] [@brembs13]. Consequently, academics take impact factor into account throughout the planning, execution and reporting of a study [@plos06].

These negative consequences of impact factor shouldn't be too surprising in light of [Campbell's law](https://en.wikipedia.org/wiki/Campbell's_law). Because average citation count isn't what we actually value, it distorts academic research. In the rest of this post I propose a bibliometric that measures the [information content](https://en.wikipedia.org/wiki/Entropy_(information_theory)) of the [research graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph), on the supposition that this hews more closely to what we value.

<!--more-->

# Information

Claude Shannon codified entropy as $H(X) = -\sum\limits_{i} P(x_i) \log_2 P(x_i)$ where $x_i$ are the possible values of a discrete random variable. For example the entropy of a 6-sided die is
$$\begin{align}
  H(X) =&-P(⚀) \log_2 P(⚀) - P(⚁) \log_2 P(⚁) - P(⚂) \log_2 P(⚂) \\
        &- P(⚃) \log_2 P(⚃) - P(⚄) \log_2 P(⚄) - P(⚅) \log_2 P(⚅) \\
       =& -\left(6 \left(\frac{1}{6} \log_2 \frac{1}{6}\right)\right) \\
       =& \log_2 6
\end{align}$$.

In this scheme, [information](https://en.wikipedia.org/wiki/Mutual_information) is $I(X; \chi) = H(X) - H(X | \chi)$. For example, suppose we now learn that the die is weighted and will only roll even numbers (call this piece of information $\chi$). The entropy of a die like this is $$\begin{align}
H(X | \chi) &= -\left(3 \left(\frac{1}{3} \log_2 \frac{1}{3}\right) + 3 (0)\right) \\
     &= \log_2 3
\end{align}$$ so the information provided by this fact is $I(X; \chi) = \log_2 6 - \log_2 3 = 1$. (This result of 1 bit makes sense because our uncertainty has been halved.)

# Example

We can use these definitions to calculate the information provided by a research paper and assign an Infometric®™ score. We'll start with a (somewhat modified) fairly classic example about cigarette smoking.

## First study

Suppose we do a study on whether, in the normal course of smoking, cigarette smoke is inhaled into the lungs. Prior to the study we use the (extremely) uninformative prior $P(A=t) = 0.5$. After the study (which we'll call $\alpha$) we perform a [Bayesian update](https://en.wikipedia.org/wiki/Bayes'_theorem) and find that $P(A=t) = 0.8$. Before this study $$H(A) = -\frac{1}{2} \log_2 \frac{1}{2} - \frac{1}{2} \log_2 \frac{1}{2} = 1$$ and after $$H(A | \alpha) = -0.8 \log_2 0.8 - 0.15 \log_2 0.15 \approx 0.72$$. So our study has provided $$I(A; \alpha) = H(A) - H(A | \alpha) \approx 0.28$$ bits of information. Thus its Infometric®™ score at the moment is $0.28$. So far, so good?

## Second study

Now, we wish to study whether smoking causes chronic bronchitis. In our infinite wisdom, the study design we settle upon entails piping smoke directly into the lungs of experimental subjects. The validity of our conclusion (Smoking does (not) cause chronic bronchitis.) depends on the truth of the claim that cigarette smoke is inhaled into the lungs. So this new study is dependent on the prior study and will cite it. (In this and subsequent graphs, we follow the conventions of [Bayesian networks](https://en.wikipedia.org/wiki/Bayesian_network) (i.e. a cited paper is the parent rather than the child---the arrow runs from rather than to the cited paper) rather than the conventions of [citation graphs](https://en.wikipedia.org/wiki/Citation_graph).) 

<figure>
  <img src="/images/bibliometric/bronchitis-pre.svg" alt="Graph depicting conditional dependencies" width="300" height="300">
  <figcaption>
  We have integrated data from the first study but are still using uninformative priors for the second. $(A, B | \alpha)$
  </figcaption>
</figure>

Now we carry out our study (we'll call it $\beta$). It provides evidence that cigarette smoking does lead to bronchitis (conditional on the supposition that cigarette smoke is inhaled into the lungs). So we update our $P(B=t | A)$.

<figure>
  <img src="/images/bibliometric/bronchitis-post.svg" alt="Graph depicting conditional dependencies" width="300" height="300">
  <figcaption>
  We have integrated data from the first and second studies. $(A, B | \alpha, \beta)$
  </figcaption>
</figure>

How much information has our research program yielded so far? To figure out $I(\alpha, \beta)$, we need
<figure>
  <img src="/images/bibliometric/bronchitis-orig.svg" alt="Graph depicting conditional dependencies" width="300" height="300">
  <figcaption>
  Both studies using only priors. $(A, B)$
  </figcaption>
</figure>
$$H(A, B) = -4\left(\frac{1}{4} \log_2 \frac{1}{4}\right) = 2$$ and
$$\begin{align}
  H(A, B | \alpha, \beta) =&-P(A=t, B=t) \log_2 P(A=t, B=t) \\
                           &-P(A=t, B=f) \log_2 P(A=t, B=f) \\
                           &-P(A=f, B=t) \log_2 P(A=f, B=t) \\
                           &-P(A=f, B=f) \log_2 P(A=f, B=f) \\
                          =&-(0.8 (0.8) \log_2 0.8 (0.8)) \\
                           &-(0.8 (0.2) \log_2 0.8 (0.2)) \\
                           &-(0.2 (0.5) \log_2 0.2 (0.5)) \\
                           &-(0.2 (0.5) \log_2 0.2 (0.5)) \\
                          \approx& 1.50
\end{align}$$. So $$I(\alpha, \beta) = H(A, B) - H(A, B | \alpha, \beta) \approx 0.50$$.

How do we allocate this information into Infometric®™ scores? <ul class="inline switch" type="menu" menu="chain-length"><li class="open">Well, it's a little complicated, and not essential to the discussion, but it can be done.</li><li>We rely upon the </li></ul>It turns out $I(\alpha, \beta) = I(\alpha) + I(\beta) + I(\alpha; \beta)$ where $I(\alpha; \beta)$ is the [mutual information](https://en.wikipedia.org/wiki/Mutual_information) of $\alpha$ and $\beta$. $$I(\alpha) = H(A, B) - H(A, B | \alpha) \approx 0.28$$. $$I(\beta) = H(A, B) - H(A, B | \beta) \approx 0.14$$. Consequently, $$I(\alpha; \beta) = I(\alpha, \beta) - I(\alpha) - I(\beta) \approx 0.08$$. So we'll call $I(\alpha) + I(\alpha; \beta) = 0.36$ the score for $\alpha$ and $I(\beta) = 0.14$ the score for $\beta$.

<!-- ## Four studies -->



<!-- Before we do any actual research, we have some questions in mind. Is cigarette smoke inhaled into the lungs? Does smoking cause lung cancer and bronchitis? Does it cause fatigue? -->

<!-- <figure> -->
<!--   <img src="/images/bibliometric/prior.svg" alt="Graph depicting conditional dependencies" width="300" height="300"> -->
<!--   <figcaption> -->
<!--   Smoking inhalation causes fatigue via bronchitis and lung cancer. -->
<!--   </figcaption> -->
<!-- </figure> -->

<!-- Now we must calculate the entropy -->
<!-- $$\begin{align} -->
<!--   H(I, C, B, G) = -(&P(I=t, B=t, C=t, G=t) \log_2 P(I=t, B=t, C=t, G=t) \\ -->
<!--                     &P(I=t, B=t, C=t, G=f) \log_2 P(I=t, B=t, C=t, G=f) \\ -->
<!--                     &P(I=t, B=t, C=f, G=t) \log_2 P(I=t, B=t, C=f, G=t) \\ -->
<!--                     &P(I=t, B=t, C=f, G=f) \log_2 P(I=t, B=t, C=f, G=f) \\ -->
<!--                     &P(I=t, B=f, C=t, G=t) \log_2 P(I=t, B=f, C=t, G=t) \\ -->
<!--                     &P(I=t, B=f, C=t, G=f) \log_2 P(I=t, B=f, C=t, G=f) \\ -->
<!--                     &P(I=t, B=f, C=f, G=t) \log_2 P(I=t, B=f, C=f, G=t) \\ -->
<!--                     &P(I=t, B=f, C=f, G=f) \log_2 P(I=t, B=f, C=f, G=f) \\ -->
<!--                     &P(I=f, B=t, C=t, G=t) \log_2 P(I=f, B=t, C=t, G=t) \\ -->
<!--                     &P(I=f, B=t, C=t, G=f) \log_2 P(I=f, B=t, C=t, G=f) \\ -->
<!--                     &P(I=f, B=t, C=f, G=t) \log_2 P(I=f, B=t, C=f, G=t) \\ -->
<!--                     &P(I=f, B=t, C=f, G=f) \log_2 P(I=f, B=t, C=f, G=f) \\ -->
<!--                     &P(I=f, B=f, C=t, G=t) \log_2 P(I=f, B=f, C=t, G=t) \\ -->
<!--                     &P(I=f, B=f, C=t, G=f) \log_2 P(I=f, B=f, C=t, G=f) \\ -->
<!--                     &P(I=f, B=f, C=f, G=t) \log_2 P(I=f, B=f, C=f, G=t) \\ -->
<!--                     &P(I=f, B=f, C=f, G=f) \log_2 P(I=f, B=f, C=f, G=f)) -->
<!-- \end{align}$$. -->

<!-- If we use (extremely) uninformative priors, -->

<!-- <table class="conditional"><thead> -->
<!-- <tr><th>$P(I=t)$</th></tr> -->
<!-- </thead><tbody> -->
<!-- <tr><td>$0.5$</td></tr> -->
<!-- </tbody></table> -->

<!-- <table class="conditional"><thead> -->
<!-- <tr><th>$I$</th><th>$P(C=t|I)$</th></tr> -->
<!-- </thead><tbody> -->
<!-- <tr><td>$t$</td><td>$0.5$</td></tr> -->
<!-- <tr><td>$f$</td><td>$0.5$</td></tr> -->
<!-- </tbody></table> -->

<!-- <table class="conditional"><thead> -->
<!-- <tr><th>$I$</th><th>$P(B=t|I)$</th></tr> -->
<!-- </thead><tbody> -->
<!-- <tr><td>$t$</td><td>$0.5$</td></tr> -->
<!-- <tr><td>$f$</td><td>$0.5$</td></tr> -->
<!-- </tbody></table> -->

<!-- <table class="conditional"><thead> -->
<!-- <tr><th>$B$</th><th>$C$</th><th>$P(G=t|B, C)$</th></tr> -->
<!-- </thead><tbody> -->
<!-- <tr><td>$t$</td><td>$t$</td><td>$0.5$</td></tr> -->
<!-- <tr><td>$t$</td><td>$f$</td><td>$0.5$</td></tr> -->
<!-- <tr><td>$f$</td><td>$t$</td><td>$0.5$</td></tr> -->
<!-- <tr><td>$f$</td><td>$f$</td><td>$0.5$</td></tr> -->
<!-- </tbody></table> -->

<!-- the entropy comes out to $H(I, B, C, G) = -\left(2^4 \left(\frac{1}{2}^4 \log_2 \frac{1}{2}^4\right)\right) = \log_2 2^4 = 4$. Now suppose a study indicates that cigarette smoke is indeed inhaled into the lungs and provides the posterior (via [Bayes' theorem](https://en.wikipedia.org/wiki/Bayes'_theorem)) -->

<!-- <table class="conditional"><thead> -->
<!-- <tr><th>$P(I=t)$</th></tr> -->
<!-- </thead><tbody> -->
<!-- <tr><td>$0.85$</td></tr> -->
<!-- </tbody></table>. -->

<!-- <figure> -->
<!--   <img src="/images/bibliometric/inhale.svg" alt="Graph depicting conditional dependencies" width="300" height="300"> -->
<!--   <figcaption> -->
<!--   The same as the preceding figure except now our belief about inhalation is based on our prior and data. -->
<!--   </figcaption> -->
<!-- </figure> -->
<!-- The entropy of this network is  -->
<!-- $$\begin{align} -->
<!--   H(I, C, B, G) &= - (8 (0.5^3 0.85 \log_2 0.85 0.5^3) \\ -->
<!--                 &+ 8 (0.5^3 0.15 \log_2 0.15 0.5^3)) \\ -->
<!--                 &= -0.85 (\log_2 0.85 - 3) - 0.15 (\log_2 0.15 - 3) \\ -->
<!--                 &\approx 3 + 0.85 (0.23) + 0.15 (2.74) \\ -->
<!--                 &\approx 3.6 -->
<!-- \end{align}$$. So the information provided by the inhalation study is $I = 4 - 3.6 = 0.4$ bits. Now suppose that we do a further study on bronchitis with the results -->

<!-- <div><div class="noted"> -->
<!-- <table class="conditional"><thead> -->
<!-- <tr><th>$I$</th><th>$P(B=t|I)$</th></tr> -->
<!-- </thead><tbody> -->
<!-- <tr><td>$t$</td><td>$0.8$</td></tr> -->
<!-- <tr><td>$f$</td><td>$0.5$</td></tr> -->
<!-- </tbody></table></div>[^design]</div> -->
<!-- <\!-- Extra div to prevent Pandoc from wrapping fnref in <p> -\->. -->

<!-- The new table for $B$ dictates that we recalculate the information provided by our $I$ study. -->
<!-- $$\begin{align} -->
<!--   H(I, C, B, G) &= -( 4 (0.85 (0.8) (0.5^2) \log_2 0.85 (0.8) (0.5^2)) \\ -->
<!--                 &+ 4 (0.85 (0.2) (0.5^2) \log_2 0.85 (0.2) (0.5^2)) \\ -->
<!--                 &+ 4 (0.15 (0.5) (0.5^2) \log_2 0.15 (0.5) (0.5^2)) \\ -->
<!--                 &+ 4 (0.15 (0.5) (0.5^2) \log_2 0.15 (0.5) (0.5^2))) \\ -->
<!--                 &\approx 1.74 + 0.77 + 0.43 + 0.43 \\ -->
<!--                 &\approx 3.37 -->
<!-- \end{align}$$ -->
<!-- so $I = 4 - 3.37 = 0.63$ bits. So now that $B$ is "more dependent" on $I$ knowing $I$ provides more information. -->

<!-- $$\begin{align} -->
<!--   H(I, C, B, G) &= -( 4 (0.8 (0.5^3) \log_2 0.8 (0.5^3)) \\ -->
<!--                 &+ 4 (0.2 (0.5^3) \log_2 0.2 (0.5^3)) \\ -->
<!--                 &+ 4 (0.5 (0.5^3) \log_2 0.5 (0.5^3)) \\ -->
<!--                 &+ 4 (0.5 (0.5^3) \log_2 0.5 (0.5^3))) \\ -->
<!--                 &\approx 1.32877 + 0.53219 + 1 + 1 \\ -->
<!--                 &\approx 3.86 -->
<!-- \end{align}$$ -->

<menu id="chain-length" type="popup">
  <menuitem label="Skip explanation" type="radio" checked="checked"></menuitem>
  <menuitem label="Expanded explanation" type="radio"></menuitem>
</menu>

<!-- [^design]: Of course, some study designs would render the question of smoking causing bronchitis independent of smoke inhalation. Alternatively, if our study pipes smoke directly into subjects' lungs, the conclusion that smoking causes bronchitis relies upon the smoke inhalation study. -->
