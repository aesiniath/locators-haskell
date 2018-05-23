--
-- Human exchangable identifiers and locators
--
-- Copyright © 2011-2018 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--
-- This code originally licenced GPLv2. Relicenced BSD3 on 2 Jan 2014.
--

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Locator.Latin25
  ( Latin25(..)
  , toLatin25
  , fromLatin25
  , hashStringToLatin25
  ) where

import Prelude hiding (toInteger)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.List (mapAccumL)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showIntAtBase)

import Data.Locator.Common

--
-- | A symbol set with twenty-five visually distinct characters.
--
-- These are not protected against similar pronounciations; if you need to
-- read your identifiers /aloud/ use 'English16' instead.
--
{-

    --  | Two       -- Obvious conflict with Z
    --  | Five      -- Obvious conflict with S
    --  | Six       -- Too close to G
    --  | Bravo     -- Too close to 8
    --  | Delta     -- Shape of D too close to O
    --  | Foxtrot   -- A bit close to E, and since we've included S, skip
    --  | India     -- Too close to 1 and J
    --  | Oscar     -- Obvious conflict with 0
    --  | Quebec    -- The tail on Q is too easy to miss, thereby colliding with O/0
    --  | Romeo     -- Dropped in favour of P
    --  | Uniform   -- Too close to V

-}
data Latin25
    = Zero'     -- ^ @\'0\'@ /0th/
    | One'      -- ^ @\'1\'@ /1st/
    | Three'    -- ^ @\'3\'@ /2nd/
    | Four'     -- ^ @\'4\'@ /3rd/
    | Seven'    -- ^ @\'7\'@ /4th/
    | Eight'    -- ^ @\'8\'@ /5th/
    | Nine'     -- ^ @\'9\'@ /6th/
    | Alpha'    -- ^ @\'A\'@ /7th/
    | Charlie'  -- ^ @\'C\'@ /8th/
    | Echo'     -- ^ @\'E\'@ /9th/
    | Golf'     -- ^ @\'G\'@ /10th/
    | Hotel'    -- ^ @\'H\'@ /11th/
    | Juliet'   -- ^ @\'J\'@ /12th/
    | Kilo'     -- ^ @\'K\'@ /13th/
    | Lima'     -- ^ @\'L\'@ /14th/
    | Mike'     -- ^ @\'M\'@ /15th/
    | November' -- ^ @\'N\'@ /16th/
    | Papa'     -- ^ @\'P\'@ /17th/
    | Sierra'   -- ^ @\'S\'@ /18th/
    | Tango'    -- ^ @\'T\'@ /19th/
    | Victor'   -- ^ @\'V\'@ /20th/
    | Whiskey'  -- ^ @\'W\'@ /21st/
    | XRay'     -- ^ @\'X\'@ /22nd/
    | Yankee'   -- ^ @\'Y\'@ /23rd/
    | Zulu'     -- ^ @\'Z\'@ /24th/
    deriving (Eq, Ord, Enum, Bounded)

instance Locator Latin25 where
    locatorToDigit x =
        case x of
            Zero'   -> '0'
            One'    -> '1'
            Three'  -> '3'
            Four'   -> '4'
            Seven'  -> '7'
            Eight'  -> '8'
            Nine'   -> '9'
            Alpha'  -> 'A'
            Charlie' -> 'C'
            Echo'   -> 'E'
            Golf'   -> 'G'
            Hotel'  -> 'H'
            Juliet' -> 'J'
            Kilo'   -> 'K'
            Lima'   -> 'L'
            Mike'   -> 'M'
            November' -> 'N'
            Papa'   -> 'P'
            Sierra' -> 'S'
            Tango'  -> 'T'
            Victor' -> 'V'
            Whiskey'-> 'W'
            XRay'   -> 'X'
            Yankee' -> 'Y'
            Zulu'   -> 'Z'

    digitToLocator :: Char -> Latin25
    digitToLocator c =
        case c of
            '0' -> Zero'
            '1' -> One'
            '3' -> Three'
            '4' -> Four'
            '7' -> Seven'
            '8' -> Eight'
            '9' -> Nine'
            'A' -> Alpha'
            'C' -> Charlie'
            'E' -> Echo'
            'G' -> Golf'
            'H' -> Hotel'
            'J' -> Juliet'
            'K' -> Kilo'
            'L' -> Lima'
            'M' -> Mike'
            'N' -> November'
            'P' -> Papa'
            'S' -> Sierra'
            'T' -> Tango'
            'W' -> Whiskey'
            'V' -> Victor'
            'X' -> XRay'
            'Y' -> Yankee'
            'Z' -> Zulu'
            _   -> error "Illegal digit"


instance Show Latin25 where
    show x = [c]
      where
        c = locatorToDigit x

--
-- | Given a number, convert it to a string in the Latin25 base 25 symbol
-- alphabet. This is useful for primary keys and object identifiers that you
-- need to scan for in log output, for example.
--
toLatin25 :: Int -> String
toLatin25 x =
    showIntAtBase 25 (represent Zulu') x ""

--
-- | Given a number encoded in Locator16, convert it back to an integer.
--
fromLatin25 :: String -> Int
fromLatin25 ss =
    foldl (multiply Zulu') 0 ss

--
-- | Take an arbitrary sequence of bytes, hash it with SHA1, then format as a
-- short @limit@-long Latin25 string.
--
-- >>> hashStringToLatin25 6 "Hello World"
-- M48HR0
--
hashStringToLatin25 :: Int -> ByteString -> ByteString
hashStringToLatin25 limit s' =
  let
    s  = S.unpack s'
    n  = digest s               -- SHA1 hash
    r  = mod n upperBound       -- trim to specified number of base 25 chars
    x  = toLatin25 r            -- express in Latin25
    b' = S.pack x
  in
    b'
  where
    upperBound = 25 ^ limit
{-
    ls = convert n' (replicate limit minBound)       :: [English16]
    (_,us) = mapAccumL uniq Set.empty ls
-}
