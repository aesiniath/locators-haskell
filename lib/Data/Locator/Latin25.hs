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
  , toLatin25a
  , fromLatin25
  , hashStringToLatin25a
  ) where

import Prelude hiding (toInteger)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.List (mapAccumL)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showIntAtBase)

import Data.Locator.Common

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
--  | Romeo     -- Dropped in favour of P
    | Sierra    -- ^ @\'S\'@ /18th/
    | Tango     -- ^ @\'T\'@ /19th/
--  | Uniform   -- Too close to V
    | Victor    -- ^ @\'V\'@ /20th/
    | Whiskey   -- ^ @\'W\'@ /21st/
    | XRay      -- ^ @\'X\'@ /22nd/
    | Yankee    -- ^ @\'Y\'@ /23rd/
    | Zulu      -- ^ @\'Z\'@ /24th/
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
            Sierra  -> 'S'
            Tango   -> 'T'
            Victor  -> 'V'
            Whiskey -> 'W'
            XRay    -> 'X'
            Yankee  -> 'Y'
            Zulu    -> 'Z'

    digitToLocator :: Char -> Latin25
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

--
-- | Given a number, convert it to a string in the Locator16 base 16 symbol
-- alphabet. You can use this as a replacement for the standard \'0\'-\'9\'
-- \'A\'-\'F\' symbols traditionally used to express hexidemimal, though really
-- the fact that we came up with 16 total unique symbols was a nice
-- co-incidence, not a requirement.
--
toLatin25 :: Int -> String
toLatin25 x =
    showIntAtBase 25 (represent Zulu) x ""

--
-- | Represent a number in Latin25a format. This uses the Latin25 symbol
-- set, and additionally specifies that no symbol can be repeated. The /a/ in
-- Latin25a represents that this transformation is done on the cheap; when
-- converting if we end up with \'9\' \'9\' we simply pick the subsequent digit
-- in the enum, in this case getting you \'9\' \'A\'.
--
-- Note that the transformation is /not/ reversible. A number like @4369@
-- (which is @0x1111@, incidentally) encodes as @12C4@. So do @4370@, @4371@,
-- and @4372@. The point is not uniqueness, but readibility in adverse
-- conditions. So while you can count locators, they don't map continuously to
-- base10 integers.
--
-- The first argument is the number of digits you'd like in the locator; if the
-- number passed in is less than 25^limit, then the result will be padded.
--
-- >>> toEnglish25a 6 4369
-- FIXME
--
toLatin25a :: Int -> Int -> String
toLatin25a limit n =
  let
    n' = abs n
    ls = convert n' (replicate limit minBound)       :: [Latin25]
    (_,us) = mapAccumL uniq Set.empty ls
  in
    map locatorToDigit (take limit us)
  where
    convert :: Locator α => Int -> [α] -> [α]
    convert 0 xs = xs
    convert i xs =
      let
        (d,r) = divMod i 16
        x = toEnum r
      in
        convert d (x:xs)

    uniq :: Locator α => Set α -> α -> (Set α, α)
    uniq s x =
        if Set.member x s
            then uniq s (subsequent x)
            else (Set.insert x s, x)

    subsequent :: Locator α => α -> α
    subsequent x =
        if x == maxBound
            then minBound
            else succ x

--
-- | Given a number encoded in Locator16, convert it back to an integer.
--
fromLatin25 :: String -> Int
fromLatin25 ss =
    foldl (multiply Zulu) 0 ss

--
-- | Take an arbitrary sequence of bytes, hash it with SHA1, then format as a
-- short @digits@-long English25 string.
--
-- >>> hashStringToLatin25a 6 "Hello World"
-- M48HR0
--
hashStringToLatin25a :: Int -> ByteString -> ByteString
hashStringToLatin25a limit s' =
  let
    s  = S.unpack s'
    n  = digest s               -- SHA1 hash
    r  = mod n upperBound       -- trim to specified number of base 25 chars
    x  = toLatin25a limit r     -- express in Latin25
    b' = S.pack x
  in
    b'
  where
    upperBound = 25 ^ limit

