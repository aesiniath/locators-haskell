--
-- Human exchangable identifiers and locators
--
-- Copyright © 2011-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--
-- This code originally licenced GPLv2. Relicenced BSD3 on 2 Jan 2014.
--

{-# LANGUAGE OverloadedStrings #-}

--
-- |
-- Maintainer: Andrew Cowie
-- Stability: Experimental
--
-- /Background/
--
-- We had a need for identifiers that could be used by humans.
--
-- The requirement to be able to say these over the phone complicates matters.
-- Most people have approached this problem by using a phonetic alphabet. The
-- trouble comes when you hear people saying stuff like \"A as in ... uh,
-- Apple?\" (should be Alpha, of course) and \"U as in ... um, what's a word
-- that starts with U?\" It gets worse. Ever been to a GPG keysigning? Listen
-- to people attempt to read out the digits of their key fingerprints. ...C 3 E
-- D  0 0 0 0  0 0 0 2 B D B D... \"Did you say \'C\' or \'D\'?\" and \"how
-- many zeros was that?\" Brutal.
--
-- So what we need is a symbol set where each digit is unambigious and doesn't
-- collide with the phonetics of another symbol. This package provides
-- Locator16, a set of 16 letters and numbers that, when spoken in English,
-- have unique pronounciation.
--
-- Also included is code to work in base 62, which is simply @[\'0\'@-@\'9\'@,
-- @\'A\'@-@\'Z\'@, and @\'a\'@-@\'z\']@. These are frequently used to express
-- short codes in URL redirectors; you may find them a more useful encoding for
-- expressing numbers than base 16 hexidecimal.
--
module Data.Locator
(
    -- * Locator16
    -- | This was somewhat inspired by the record locators used by the civilian
    -- air travel industry, but with the restriction that the symbol set is
    -- carefully chosen (aviation locators do heroic things like excluding
    -- \'I\' but not much else) and, in the case of Locator16a, to not repeat
    -- symbols. They're not a reversable encoding, but assuming you're just
    -- generating identifiers and storing them somewhere, they're quite handy.
    --
    -- @TODO@ /link to paper with pronunciation study when published./
    --
    Locator(..),
    English16(..),
    fromLocator16,
    toLocator16,
    toLocator16a,
    hashStringToLocator16a,

    -- * Base62
    toBase62,
    fromBase62,
    hashStringToBase62

) where

import Data.Locator.Hashes
import Data.Locator.Locators
