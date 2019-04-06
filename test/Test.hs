{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.HUnit (assertEqual, Counts, runTestTT, Test(TestCase, TestList))
import Test.HUnit.Lang (Assertion)

import NaiveBayes (Label(..), Proba(..), examples, mapLabels, mapProba, tally,
    tokenize, toProba)

testTokenize :: Assertion
testTokenize =
    assertEqual
        "assertEqual tokenize"
        (tokenize "5% Guaranteed for Eight Years")
        ["5", "guaranteed", "for", "eight", "years"]

testsTally :: [Assertion]
testsTally =
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
    extract (Label (a, b, c)) = (a, b, c)

testsPipeline :: [Assertion]
testsPipeline =
    [ assertEqual
        "assertEqual map show $ mapLabels examples"
        (map show $ mapLabels examples)
        [ "Label (1,0,\"5\")"
        , "Label (0,2,\"absurdities\")"
        , "Label (0,1,\"and\")"
        , "Label (1,0,\"any\")"
        , "Label (1,0,\"copy\")"
        , "Label (1,0,\"dvd\")"
        , "Label (1,0,\"eight\")"
        , "Label (1,0,\"for\")"
        , "Label (1,0,\"friend\")"
        , "Label (1,0,\"game\")"
        , "Label (1,0,\"guaranteed\")"
        , "Label (0,1,\"ideas\")"
        , "Label (0,1,\"in\")"
        , "Label (0,2,\"life\")"
        , "Label (0,1,\"news\")"
        , "Label (0,2,\"of\")"
        , "Label (1,0,\"or\")"
        , "Label (0,1,\"patented\")"
        , "Label (1,0,\"playstation\")"
        , "Label (0,3,\"re\")"
        , "Label (0,2,\"sa\")"
        , "Label (0,1,\"satalk\")"
        , "Label (1,0,\"software\")"
        , "Label (0,3,\"the\")"
        , "Label (1,0,\"this\")"
        , "Label (0,1,\"was\")"
        , "Label (1,0,\"with\")"
        , "Label (1,0,\"years\")"
        ]
    , assertEqual
        "assertEqual mapLabels []"
        (mapLabels [])
        []
    ]

testToProba :: Assertion
testToProba =
    assertEqual
        "toProba ..."
        (toProba k (Label (10, 5, "[TOTAL]")) (Label (1, 3, "word")))
        $ Proba ((1 + k) / (10 + (2 * k)), (3 + k) / (5 + (2 * k)), "word")
  where
    k = 1

testMapProba :: Assertion
testMapProba =
    assertEqual
        "mapProba ..."
        (show $ mapProba 1 [Label (1, 5, "foo"), Label (5, 9, "bar")])
        "[Proba (0.25,0.375,\"foo\"),Proba (0.75,0.625,\"bar\")]"

main :: IO Counts
main = (runTestTT . TestList . map TestCase) tests
  where
    tests =
        [testTokenize, testToProba, testMapProba]
        ++ testsTally
        ++ testsPipeline
