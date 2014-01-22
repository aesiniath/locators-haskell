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

module Data.Locator
(
    hashStringToBase62,

    Locator(..),
    English16(..),
    fromLocator16,
    toLocator16,
    toLocator16a,
    hashStringToLocator16a
) where

import Data.Locator.Hashes
import Data.Locator.Locators
