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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Locator.Latin25
(
    Latin25(..),
    toLocator25,
    toLocator25a,
    hashStringToLocator25a
) where


import Prelude hiding (toInteger)

import Crypto.Hash.SHA1 as Crypto
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S

import Data.Locator.Locators

data Latin25
    = Zero      -- ^ @\'0\'@ /0th/
    | One       -- ^ @\'1\'@ /1st/
--  | Two       -- Obvious conflict with Z
    | Three     -- ^ @\'3\'@ /2nd/
    | Four      -- ^ @\'4\'@ /3rd/
--  | Five      -- Obvious conflict with S
--  | Six       -- Too close to G
    | Seven     -- ^ @\'7\'@ /4th/
    | Eight     -- ^ @\'8\'@ /5th/
    | Nine      -- ^ @\'9\'@ /6th/

    | Alpha     -- ^ @\'A\'@ /7th/
--  | Bravo     -- Too close to 8
    | Charlie   -- ^ @\'C\'@ /8th/
--  | Delta     -- Shape of D too close to O
    | Echo      -- ^ @\'E\'@ /9th/
--  | Foxtrot   -- A bit close to E, and since we've included S, skip
    | Golf      -- ^ @\'G\'@ /10th/
    | Hotel     -- ^ @\'H\'@ /11th/
--  | India     -- Too close to 1 and J
    | Juliet    -- ^ @\'J\'@ /12th/
    | Kilo      -- ^ @\'K\'@ /13th/
    | Lima      -- ^ @\'L\'@ /14th/
    | Mike      -- ^ @\'M\'@ /15th/
    | November  -- ^ @\'N\'@ /16th/
--  | Oscar     -- Obvious conflict with 0
    | Papa      -- ^ @\'P\'@ /17th/
--  | Quebec    -- The tail on Q is too easy to miss, thereby colliding with O/0
    | Romeo     -- ^ @\'R\'@ /18th/
    | Sierra    -- ^ @\'S\'@ /19th/
    | Tango     -- ^ @\'T\'@ /20th/
--  | Uniform   -- Too close to V
    | Victor    -- ^ @\'V\'@ /21st/
    | Whiskey   -- ^ @\'W\'@ /22nd/
    | XRay      -- ^ @\'X\'@ /23rd/
    | Yankee    -- ^ @\'Y\'@ /24th/
    | Zulu      -- ^ @\'Z\'@ /25th/
    deriving (Eq, Ord, Enum, Bounded)

instance Locator Latin25 where
    locatorToDigit x =
        case x of
            Zero    -> '0'
            One     -> '1'
            Three   -> '3'
            Four    -> '4'
            Seven   -> '7'
            Eight   -> '8'
            Nine    -> '9'
            Alpha   -> 'A'
            Charlie -> 'C'
            Echo    -> 'E'
            Golf    -> 'G'
            Hotel   -> 'H'
            Juliet  -> 'J'
            Kilo    -> 'K'
            Lima    -> 'L'
            Mike    -> 'M'
            November -> 'N'
            Papa    -> 'P'
            Romeo   -> 'R'
            Sierra  -> 'S'
            Tango   -> 'T'
            Victor  -> 'V'
            Whiskey -> 'W'
            XRay    -> 'X'
            Yankee  -> 'Y'
            Zulu    -> 'Z'

    digitToLocator c =
        case c of
            '0' -> Zero
            '1' -> One
            '3' -> Three
            '4' -> Four
            '7' -> Seven
            '8' -> Eight
            '9' -> Nine
            'A' -> Alpha
            'C' -> Charlie
            'E' -> Echo
            'G' -> Golf
            'H' -> Hotel
            'J' -> Juliet
            'K' -> Kilo
            'L' -> Lima
            'M' -> Mike
            'N' -> November
            'P' -> Papa
            'R' -> Romeo
            'S' -> Sierra
            'T' -> Tango
            'W' -> Whiskey
            'V' -> Victor
            'X' -> XRay
            'Y' -> Yankee
            'Z' -> Zulu
            _   -> error "Illegal digit"



instance Show Latin25 where
    show x = [c]
      where
        c = locatorToDigit x




value :: Char -> Int
value c =
    fromEnum $ (digitToLocator c :: Latin25)  -- FIXME



--
-- | Given a number, convert it to a string in the Locator16 base 16 symbol
-- alphabet. You can use this as a replacement for the standard \'0\'-\'9\'
-- \'A\'-\'F\' symbols traditionally used to express hexidemimal, though really
-- the fact that we came up with 16 total unique symbols was a nice
-- co-incidence, not a requirement.
--
toLocator25 :: Int -> String
toLocator25 x =
    map locatorToDigit (convertToBase 25 x [] :: [Latin25])


toLocator25a :: Int -> Int -> String
toLocator25a limit n =
  let
    symbols = toLocatorUnique limit n :: [Latin25]
  in
    map locatorToDigit symbols


multiply :: Int -> Char -> Int
multiply acc c =
    acc * 25 + value c

--
-- | Given a number encoded in Locator16, convert it back to an integer.
--
fromLatin25 :: String -> Int
fromLatin25 ss =
    foldl multiply 0 ss


--
-- Given a string of bytes, cast it back to a number
--

concatToInteger :: ByteString -> Int
concatToInteger bytes =
    B.foldl fn 0 bytes
  where
    fn acc b = (acc * 256) + (fromIntegral b)

digest :: ByteString -> Int
digest x' =
    i
  where
    i  = concatToInteger h'
    h' = Crypto.hash x'


hashStringToLocator25a :: Int -> ByteString -> ByteString
hashStringToLocator25a limit s =
  let
    n  = digest s               -- SHA1 hash
    r  = mod n upperBound       -- trim to specified number of base 25 chars
    x  = toLocator25a limit r     
    b' = S.pack x
  in
    b'
  where
    upperBound = 25 ^ limit

