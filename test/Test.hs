{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.HUnit (assertEqual, Counts, runTestTT, Test(TestCase, TestList))
import Test.HUnit.Lang (Assertion)

import NaiveBayes (Label(..), classify, corpus, exTrue, exFalse, mapLabels,
    mapProba, tally, tokenize, toProba, train)

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

testsMapLabels :: [Assertion]
testsMapLabels =
    [ assertEqual
        "assertEqual map show $ mapLabels corpus"
        (map show $ mapLabels $ take 5 corpus)
            [ "Label (1,0,\"5\")"
            , "Label (2,0,\"a\")"
            , "Label (1,0,\"commission\")"
            , "Label (1,0,\"congratulations\")"
            , "Label (1,0,\"eight\")"
            , "Label (2,0,\"for\")"
            , "Label (2,0,\"free\")"
            , "Label (1,0,\"get\")"
            , "Label (1,0,\"guaranteed\")"
            , "Label (1,0,\"handheld\")"
            , "Label (1,0,\"it\")"
            , "Label (1,0,\"kind\")"
            , "Label (1,0,\"level\")"
            , "Label (1,0,\"maker\")"
            , "Label (1,0,\"marketing\")"
            , "Label (1,0,\"money\")"
            , "Label (1,0,\"next\")"
            , "Label (1,0,\"of\")"
            , "Label (2,0,\"one\")"
            , "Label (1,0,\"organizer\")"
            , "Label (1,0,\"sale\")"
            , "Label (1,0,\"streams\")"
            , "Label (1,0,\"take\")"
            , "Label (1,0,\"the\")"
            , "Label (1,0,\"three\")"
            , "Label (1,0,\"to\")"
            , "Label (1,0,\"try\")"
            , "Label (1,0,\"years\")"
            , "Label (1,0,\"you\")"
            , "Label (1,0,\"your\")"
            ]
    , assertEqual
        "assertEqual mapLabels []"
        (mapLabels [])
        []
    ]

testToProba :: Assertion
testToProba =
    assertEqual
        "toProba k (8, 6) (Label (1, 3, \"word\")"
        (show $ toProba k (8, 6) (Label (1, 3, "word")))
        "Proba (0.2,0.5,\"word\")"
  where
    k = 1

testMapProba :: Assertion
testMapProba =
    assertEqual
        "mapProba ..."
        (show $ mapProba 1 (6, 14) [Label (1, 5, "foo"), Label (5, 9, "bar")])
        "[Proba (0.25,0.375,\"foo\"),Proba (0.75,0.625,\"bar\")]"

testTrain :: Assertion
testTrain =
    assertEqual
        "train (take 5 corpus)"
        (map show $ train (take 5 corpus))
            [ "Proba (0.25,0.5,\"5\")"
            , "Proba (0.41666666,0.5,\"a\")"
            , "Proba (0.25,0.5,\"commission\")"
            , "Proba (0.25,0.5,\"congratulations\")"
            , "Proba (0.25,0.5,\"eight\")"
            , "Proba (0.41666666,0.5,\"for\")"
            , "Proba (0.41666666,0.5,\"free\")"
            , "Proba (0.25,0.5,\"get\")"
            , "Proba (0.25,0.5,\"guaranteed\")"
            , "Proba (0.25,0.5,\"handheld\")"
            , "Proba (0.25,0.5,\"it\")"
            , "Proba (0.25,0.5,\"kind\")"
            , "Proba (0.25,0.5,\"level\")"
            , "Proba (0.25,0.5,\"maker\")"
            , "Proba (0.25,0.5,\"marketing\")"
            , "Proba (0.25,0.5,\"money\")"
            , "Proba (0.25,0.5,\"next\")"
            , "Proba (0.25,0.5,\"of\")"
            , "Proba (0.41666666,0.5,\"one\")"
            , "Proba (0.25,0.5,\"organizer\")"
            , "Proba (0.25,0.5,\"sale\")"
            , "Proba (0.25,0.5,\"streams\")"
            , "Proba (0.25,0.5,\"take\")"
            , "Proba (0.25,0.5,\"the\")"
            , "Proba (0.25,0.5,\"three\")"
            , "Proba (0.25,0.5,\"to\")"
            , "Proba (0.25,0.5,\"try\")"
            , "Proba (0.25,0.5,\"years\")"
            , "Proba (0.25,0.5,\"you\")"
            , "Proba (0.25,0.5,\"your\")"
            ]

testsClassify :: [Assertion]
testsClassify =
    [ assertEqual
        "classify exTrue $ train corpus"
        (show $ classify exTrue $ train corpus)
        "(\"Online Doctors will fill your Viagra Prescription Now!!! QEEB\",\
            \0.8480228)"
    , assertEqual
        "classify exFalse $ train corpus"
        (show $ classify exFalse $ train corpus)
        "(\"RE: Microsoft buys XDegress - more of a p2p/distributed data \
            \thing\",4.7495705e-3)"
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) tests
  where
    tests =
        [testTokenize, testToProba, testMapProba, testTrain]
        ++ testsTally
        ++ testsMapLabels
        ++ testsClassify
