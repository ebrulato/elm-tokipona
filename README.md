# elm-tokipona

This package aims to help you in tokipona translation.
Note that tokipona is a very ambiguous language :)

It supports :
* lesson 1 is an introduction
* lesson 2 is about the alphabet and the pronunciation.
* lesson 3 http://tokipona.net/tp/janpije/okamasona3.php
* lesson 4 http://tokipona.net/tp/janpije/okamasona4.php
* lesson 5 http://tokipona.net/tp/janpije/okamasona5.php
  * Ambiguous : *lukin* or *wile* can act as a pre verb, but we want *sewi*, *lili* and *mute* to act as some adverb... so I added a special rule for this case

* lesson 6 http://tokipona.net/tp/janpije/okamasona6.php
If we have these pattern:
  * ***(wile or kama) (preposition)*** => *wile* or *kama* must be a pre verb and the preposition is nearly a standard verb, where the adverbs are some nouns.

* lesson 7 http://tokipona.net/tp/janpije/okamasona7.php
* lesson 8 http://tokipona.net/tp/janpije/okamasona8.php
  * ***(preverb) (kepeken)*** => *verb* + *kepeken* as a preposition if there are some ohter word
