{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}

-- Tests with length-typed treetors. To run:
-- 
--   hermit Test.hs -v0 -opt=DictTest DoTest.hss

module Test where

-- Odd: Without a mention of succ somewhere in this module, buildDictionary
-- fails silently, producing no bindings.
foo :: ()
foo = succ ()

test1 :: Bool
test1 = False

test2 :: Int
test2 = 3

-- The 'succ' transformation should fail for this one. Instead, buildDictionary
-- fails silently, producing no bindings.
test3 :: String
test3 = "oops!"
