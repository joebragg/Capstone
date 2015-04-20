Final Capstone Report
========================================================
author: Joe Bragg
date: April 19, 2015
font-family: 'Helvetica'
transition: rotate

Project Objectives
========================================================
The objective of this project is to build a Shiny application that will predict the next word based on a phase of one or more words entered in a text box.

As mentioned in the [Milestone Report](http://rpubs.com/jbragg/69370), the raw data from [HC Corpora](http://www.corpora.heliohost.org) was cleaned, sampled and converted into tables of NGrams of 1 to 5 words.

How does it work?
========================================================
Prediction is accomplished using the Markov Chain Rule on the NGrams which basically states the next word $(W_i)$ can be predicted by only looking at the last few n words $(W_{i-n}...W_{i-1})$ in the sentence. Therefore, the probability of $(W_i)$ given the previous n words is:

$$
P(W_i|W_{i-n}...W_{i-1}) = \frac{Count(W_{i-n}...W_i)}{Count(W_{i-n}...W_{i-1})}
$$
$$
\text{ where }n = 1,2,3,4,5
$$



How to use the application?
========================================================


Summary
========================================================
