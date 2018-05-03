--
-- Human exchangable identifiers and locators
--
-- Copyright © 2013-2017 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

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
        testProblematicEdgeCases
        testNegativeNumbers

    describe "Hashes" $ do
        testPaddingRefactored


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
        toLocator16a 6 0x111111 `shouldBe` "12C4FH"
        toLocator16a 6 0x777777 `shouldBe` "789KLM"
        toLocator16a 6 0xCCCCCC `shouldBe` "MRXY01"

testProblematicEdgeCases =
    it "converstion to Locator16a correct on corner cases" $ do
        toLocator16a 6 0x0 `shouldBe` "012C4F"
        hashStringToLocator16a 6 "perf_data" `shouldBe` "FHL417"
        hashStringToLocator16a 6 "perf_data/bletchley" `shouldBe` "K48F01"

testPaddingRefactored =
    it "correctly pads strings" $ do
        padWithZeros 5 "1" `shouldBe` "00001"
        padWithZeros 5 "123456" `shouldBe` "123456"
        (padWithZeros 11 . toBase62 $ 2^64) `shouldBe` "LygHa16AHYG"
        (hashStringToBase62 11 . S.pack . show $ 2^64) `shouldBe` "k8SQgkJtxLo"

testNegativeNumbers =
    it "doesn't explode if fed a negative number" $ do
        toLocator16a 1 (-1) `shouldBe` "1"
