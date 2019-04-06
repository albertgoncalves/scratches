{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.HUnit (assertEqual, Counts, runTestTT, Test(TestCase, TestList))
import Test.HUnit.Lang (Assertion)

import NaiveBayes (Labels(..), tally, tokenize, examples, pipeline)

test_tokenize :: Assertion
test_tokenize =
    assertEqual
        "assertEqual tokenize"
        (tokenize "5% Guaranteed for Eight Years")
        ["5", "guaranteed", "for", "eight", "years"]

test_tally :: Assertion
test_tally =
    assertEqual
        "assertEqual tally True"
        (tally True "a")
        (Labels (1, 0, "a"))

tests :: Test
tests = TestList $ map TestCase [test_tokenize, test_tally]

main :: IO Counts
main =
    mapM_ print (pipeline $ take 1 examples) >>
    runTestTT tests
