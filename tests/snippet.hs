--
-- Human exchangable identifiers and locators
--
-- Copyright © 2013-2014 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module Main where

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Prelude hiding (max)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word (Word8, Word32, Word64)
import Debug.Trace

--
-- What we're testing
--


import Data.Locator

main = do
    let n1 = 0x1111111111111111
    putStrLn ""
    putStrLn $ toLocator16  n1
    putStrLn $ toLocator16a n1
    putStrLn ""
