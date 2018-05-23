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
    = Zero_     -- ^ @\'0\'@ /0th/
    | One_      -- ^ @\'1\'@ /1st/
    | Three_    -- ^ @\'3\'@ /2nd/
    | Four_     -- ^ @\'4\'@ /3rd/
    | Seven_    -- ^ @\'7\'@ /4th/
    | Eight_    -- ^ @\'8\'@ /5th/
    | Nine_     -- ^ @\'9\'@ /6th/
    | Alpha_    -- ^ @\'A\'@ /7th/
    | Charlie_  -- ^ @\'C\'@ /8th/
    | Echo_     -- ^ @\'E\'@ /9th/
    | Golf_     -- ^ @\'G\'@ /10th/
    | Hotel_    -- ^ @\'H\'@ /11th/
    | Juliet_   -- ^ @\'J\'@ /12th/
    | Kilo_     -- ^ @\'K\'@ /13th/
    | Lima_     -- ^ @\'L\'@ /14th/
    | Mike_     -- ^ @\'M\'@ /15th/
    | November_ -- ^ @\'N\'@ /16th/
    | Papa_     -- ^ @\'P\'@ /17th/
    | Sierra_   -- ^ @\'S\'@ /18th/
    | Tango_    -- ^ @\'T\'@ /19th/
    | Victor_   -- ^ @\'V\'@ /20th/
    | Whiskey_  -- ^ @\'W\'@ /21st/
    | XRay_     -- ^ @\'X\'@ /22nd/
    | Yankee_   -- ^ @\'Y\'@ /23rd/
    | Zulu_     -- ^ @\'Z\'@ /24th/
    deriving (Eq, Ord, Enum, Bounded)

instance Locator Latin25 where
    locatorToDigit x =
        case x of
            Zero_   -> '0'
            One_    -> '1'
            Three_  -> '3'
            Four_   -> '4'
            Seven_  -> '7'
            Eight_  -> '8'
            Nine_   -> '9'
            Alpha_  -> 'A'
            Charlie_ -> 'C'
            Echo_   -> 'E'
            Golf_   -> 'G'
            Hotel_  -> 'H'
            Juliet_ -> 'J'
            Kilo_   -> 'K'
            Lima_   -> 'L'
            Mike_   -> 'M'
            November_ -> 'N'
            Papa_   -> 'P'
            Sierra_ -> 'S'
            Tango_  -> 'T'
            Victor_ -> 'V'
            Whiskey_-> 'W'
            XRay_   -> 'X'
            Yankee_ -> 'Y'
            Zulu_   -> 'Z'

    digitToLocator :: Char -> Latin25
    digitToLocator c =
        case c of
            '0' -> Zero_
            '1' -> One_
            '3' -> Three_
            '4' -> Four_
            '7' -> Seven_
            '8' -> Eight_
            '9' -> Nine_
            'A' -> Alpha_
            'C' -> Charlie_
            'E' -> Echo_
            'G' -> Golf_
            'H' -> Hotel_
            'J' -> Juliet_
            'K' -> Kilo_
            'L' -> Lima_
            'M' -> Mike_
            'N' -> November_
            'P' -> Papa_
            'S' -> Sierra_
            'T' -> Tango_
            'W' -> Whiskey_
            'V' -> Victor_
            'X' -> XRay_
            'Y' -> Yankee_
            'Z' -> Zulu_
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
    showIntAtBase 25 (represent Zulu_) x ""

--
-- | Represent a number in Latin25a format. This uses the Latin25 symbol
-- set, and additionally specifies that no symbol can be repeated. The /a/ in
-- Latin25a represents that this transformation is done on the cheap; when
-- converting if we end up with \'9\' \'9\' we simply pick the subsequent digit
-- in the enum, in this case getting you \'9\' \'A\'.
--
-- Note that the transformation is /not/ reversible. A number like @4369@
-- (which is @0x1111@, incidentally) encodes as @1345@. So do @4370@, @4371@,
-- and @4372@. The point is not uniqueness, but readibility in adverse
-- conditions. So while you can count locators, they don't map continuously to
-- base10 integers.
--
-- The first argument is the number of digits you'd like in the locator; if the
-- number passed in is less than 25^limit, then the result will be padded.
--
-- >>> toLatin25a 5 9999
-- "3A0M1"
--
-- There are only 25 symbols available so you can't specify a limit > 25.
--
toLatin25a :: Int -> Int -> String
toLatin25a limit n
  | limit > 25 = error "Can only request a maximum of 25 English25a characters, not " ++ (show limit)
  | otherwise  =
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
    foldl (multiply Zulu_) 0 ss

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

