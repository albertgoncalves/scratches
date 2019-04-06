{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.HUnit (assertEqual, Counts, runTestTT, Test(TestCase, TestList))
import Test.HUnit.Lang (Assertion)

import NaiveBayes (Labels(..), examples, pipeline, tally, tokenize)

test_tokenize :: Assertion
test_tokenize =
    assertEqual
        "assertEqual tokenize"
        (tokenize "5% Guaranteed for Eight Years")
        ["5", "guaranteed", "for", "eight", "years"]

tests_tally :: [Assertion]
tests_tally =
    [ assertEqual
        "assertEqual tally True"
        (extract $ tally True "")
        (1, 0, "")
    , assertEqual
        "assertEqual tally False"
        (extract $ tally False "")
        (0, 1, "")
    ]
  where
    extract (Labels (a, b, c)) = (a, b, c)

test_pipeline :: Assertion
test_pipeline =
    assertEqual
        "assertEqual map show $ pipeline examples"
        (map show $ pipeline examples)
        [ "Labels (1,0,\"5\")"
        , "Labels (0,2,\"absurdities\")"
        , "Labels (0,1,\"and\")"
        , "Labels (1,0,\"any\")"
        , "Labels (1,0,\"copy\")"
        , "Labels (1,0,\"dvd\")"
        , "Labels (1,0,\"eight\")"
        , "Labels (1,0,\"for\")"
        , "Labels (1,0,\"friend\")"
        , "Labels (1,0,\"game\")"
        , "Labels (1,0,\"guaranteed\")"
        , "Labels (0,1,\"ideas\")"
        , "Labels (0,1,\"in\")"
        , "Labels (0,2,\"life\")"
        , "Labels (0,1,\"news\")"
        , "Labels (0,2,\"of\")"
        , "Labels (1,0,\"or\")"
        , "Labels (0,1,\"patented\")"
        , "Labels (1,0,\"playstation\")"
        , "Labels (0,3,\"re\")"
        , "Labels (0,2,\"sa\")"
        , "Labels (0,1,\"satalk\")"
        , "Labels (1,0,\"software\")"
        , "Labels (0,3,\"the\")"
        , "Labels (1,0,\"this\")"
        , "Labels (0,1,\"was\")"
        , "Labels (1,0,\"with\")"
        , "Labels (1,0,\"years\")"
        ]

tests :: Test
tests =
    (TestList . map TestCase)
    (test_tokenize : test_pipeline : tests_tally)

main :: IO Counts
main = runTestTT tests
