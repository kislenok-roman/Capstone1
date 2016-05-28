Type helper application
========================================================
width: 1200
height: 900
author: Roman Kislenok
date: 2014-12-02
<script type="text/javascript"
  src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

*Predict what you type in next...*

Prediction algorithm
========================================================

* Identify current text type:
  + predefined by user, or
  + select type that takes maximum current phrase probability:

  $$ \max_{type \in types} P(currentPhrase | type) $$

* Identify what we should do:
  + predict whole word if we have phrase ended by space (ex. `currentPhrase == "The "`), or
  + fill in current word (ex. `currentPhrase == "The cur"`)


Prediction algorithm, continued
=======================================================

* Try to predict requested amount ($N$) of words using previous two known words:

  $$ \max_{word_3 \in words}^N P(word_3 | type, word_1, word_2) $$

* Sometimes if we have no two previous words or predicted ($N_3$) less than $N$ words:

  $$ \max_{word_2 \in words}^{N-N_3} P(word_2 | type, word_1) $$

  or even:

  $$ \max_{word \in words}^{N-N_3-N_2} P(word | type) $$


Application
========================================================

Application [available](https://kislenok.shinyapps.io/CapstoneResult/) on shinyapps platform:

![test](app2.png)

Features
========================================================

1. Application is fast (first load is slow due to limitations of shiny)

2. Responsive UI allow access from PC, Mac, iPhone and Android!

3. It can easily be scaled up (use more complicated dictionaries with 3 or more words, include rare combinations) or down (shorten dictionaries)

4. Tunning parameters (text type, amount of predictions, case sensitivity) allows more appropriate use
