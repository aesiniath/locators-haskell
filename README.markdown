Identifiers and Locators
========================

We had a need for identifiers that could be used by humans. The requirement to
be able to say these over the phone complicated matters.

Most people approach this problem by using a phonetic alphabet. The trouble
comes when you hear people saying stuff like _"A as in ... uh, Apple?"_ (should
be Alpha, of course) and _"U as in ... um, what's a word that starts with U?"_
It gets worse. Ever been to a GPG keysigning? Listen to people attempt to read
out the digits of their key fingerprints. _"...C 3 E D 0 0 0 0 0 0 0 2 B D B
D..."_ _"Did you say C or D?"_ and _"how many zeros was that?"_ Brutal.

So what we need is a symbol set where each digit is unambigious and doesn't
collide with the phonetics of another symbol. This package provides
[Locator16][], a set of 16 letters and numbers that, when spoken in English,
have unique pronounciations.

Also included is code to work in base 62, which is simply [`'0'`-`'9'`,
`'A'`-`'Z'`, and `'a'`-`'z'`]. These are frequently used to express short codes
in URL redirectors; you may find them a more useful encoding for expressing
numbers than base 16 hexidecimal.


[Locator16]: <https://hackage.haskell.org/package/locators/docs/Data-Locator.html>
