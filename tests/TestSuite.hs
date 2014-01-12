--
-- Human exchangable identifiers and locators
--
-- Copyright © 2013-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# OPTIONS -fno-warn-orphans #-}

module TestSuite where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck (elements, property)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import qualified Data.Map.Strict as Map
import Debug.Trace

--
-- What we're actually testing.
--

import Data.Locator

suite :: Spec
suite = do
    describe "Locators" $ do
        testRoundTripLocator16
        testKnownLocator16a



testRoundTripLocator16 =
    prop "safe conversion to/from Locator16" prop_Locator16

prop_Locator16 :: Int -> Bool
prop_Locator16 i =
  let
    n = abs i
    decoded = fromLocator16 (toLocator16 n)
  in
    n == decoded


--
-- Have to do these manually, since Locator16a is not round-trip safe.
--
testKnownLocator16a =
    it "constrains Locator16a to unique digits" $ do
        assertEqual "Incorrect result" "12C4FH" (toLocator16a 0x111111)
        assertEqual "Incorrect result" "789KLM" (toLocator16a 0x777777)
        assertEqual "Incorrect result" "MRXY01" (toLocator16a 0xCCCCCC)


