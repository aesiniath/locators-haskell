Identifiers and Locators
========================
"Identifiers for Humans"

Examples
---------
* K8L2FH
* X49MKY
* YCX0H1

Background
-----------
We had a need for identifiers that could be used by humans.

The requirement to be able to say these over the phone complicates matters.
Most people have approached this problem by using a phonetic alphabet. The
trouble comes when you hear people saying stuff like "A as in ... uh,
Apple?" (should be Alpha, of course) and "U as in ... um, what's a word
that starts with U?" It gets worse. Ever been to a GPG keysigning? Listen
to people attempt to read out the digits of their key fingerprints. ...C 3 E
D  0 0 0 0  0 0 0 2 B D B D... "Did you say 'C' or 'D'?" and "how
many zeros was that?" Brutal.

So what we need is a symbol set where each digit is unambigious and doesn't
collide with the phonetics of another symbol. This package provides
Locator16, a set of 16 letters and numbers that, when spoken in English,
have unique pronounciation.

The fact there are sixteen symbols is more an indication of a certain degree
of bullheaded-ness on the part of the author, and less of any kind of actual
requirement. We might have a slighly better readback score if we dropped to
15 or 14 unique characters. It does mean you can match up with hexidecimal,
which is not entirely without merit.

Also included is code to work in base 62, which is simply ['0'-'9','A'-'Z','a'-'z']. 
These are frequently used to express short codes in URL redirectors; you may 
find them a more useful encoding for expressing numbers than base 16 
hexidecimal.

Design
-------
The grouping of letters and numbers was the hard part; having come up with
the set and deconflicted the choices, the ordering is then entirely
arbitrary. Since there are some numbers, might as well have them at the same
place they correspond to in base 10; the letters were then allocated in
alpha order in the remaining slots.


* 0: Conflicts with 'O' obviously, and 'Q' often enough
* 2: 'U', 'W', and '2'. 'W' is disqualifed because of
  the way Australians butcher double-this and triple-that. "Double
  'U'" or "'W'"?
* C: 'B', 'C', 'D', 'E', 'G', 'P', 'T',
  'V', and '3' plus 'Z' because Americans can't pronounce
  Zed properly.
* 4: '4' and '5' are often confused, and '5', definitely
  out due to its collision with 'I' when spoken and 'S' in
  writing.
* F: 'F' and 'S' are notoriously confused, making the choice of
  'F' borderline, but 'S' is already disqualified for looking
  like '5'.
* K: group of 'A', 'J', 'K'.
* L: 'L' has good phonetics, and as long as it's upper case (which
  the whole 'English16' symbol set is) there's no conflict with
  '1'.
* M: choice from 'M' and 'N'; the latter is a little too close
  to '7'.
* X: choice from 'X' and '6'.
* Y: choice from 'I', 'Y', '5'. 'I' is out for the
  usual reason of being similar to '1'.

This was somewhat inspired by the record locators used by the civilian
air travel industry, but with the restriction that the symbol set is
carefully chosen (aviation locators do heroic things like excluding
'I' but not much else) and, in the case of Locator16a, to not repeat
symbols. They're not a reversable encoding, but assuming you're just
generating identifiers and storing them somewhere, they're quite handy.

Full list of symbols
---------------------
* Zero    -> '0'
* One     -> '1'
* Two     -> '2'
* Charlie -> 'C'
* Four    -> '4'
* Foxtrot -> 'F'
* Hotel   -> 'H'
* Seven   -> '7'
* Eight   -> '8'
* Nine    -> '9'
* Kilo    -> 'K'
* Lima    -> 'L'
* Mike    -> 'M'
* Romeo   -> 'R'
* XRay    -> 'X'
* Yankee  -> 'Y'
