{-# OPTIONS_GHC -Wall #-}

module NaiveBayes where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

data Labels = Labels (Int, Int, String) deriving (Show)

instance Eq Labels where
    Labels (_, _, c) == Labels (_, _, c') = c == c'

instance Ord Labels where
    Labels (_, _, c) `compare` Labels (_, _, c') = compare c c'

instance Semigroup Labels where
    Labels (a, b, c) <> Labels (a', b', _) = Labels (a + a', b + b', c)

instance Monoid Labels where
    mempty = Labels (0, 0, mempty)
    mappend = (<>)

tokenize :: String -> [String]
tokenize =
    filter (/= "")
    . map (filter isAlphaNum)
    . words
    . map toLower

tally :: Bool -> String -> Labels
tally True = Labels . (,,) 1 0
tally False = Labels . (,,) 0 1

pipeline :: [(String, Bool)] -> [Labels]
pipeline =
    map (foldl1 mappend)
    . group
    . sort
    . concat
    . map (\(xs, b) -> map (tally b) (tokenize xs))

examples :: [(String, Bool)]
examples =
    [ ("Friend, Copy ANY DVD or Playstation Game with this software...", True)
    , ("5% Guaranteed for Eight Years", True)
    , ("Congratulations! You Get a Free Handheld Organizer!", True)
    , ("One of a kind Money maker! Try it for free!", True)
    , ("Online Doctors will fill your Viagra Prescription Now!!! QEEB", True)
    , ("Take your Marketing to the Next Level", True)
    , ("One Sale - Three Commission Streams", True)
    , ("Find Peace, Harmony, Tranquility, And Happiness Right Now!", True)
    , ("ADV: Extended Auto Warranties Here undoc", True)
    , ("Definitely the answer many have been waiting for!!", True)
    , ("Re: The case for spam", False)
    , ("[use Perl] Headlines for 2002-10-09", False)
    , ("Sun Nabs Storage Startup - buys Pirus Networks", False)
    , ("Re: [VoID] a new low on the personals tip...", False)
    , ("RE: The absurdities of life.", False)
    , ("RE: The absurdities of life.", False)
    , ("[Spambayes] timtest broke?", False)
    , ("public mailing list sign up package", False)
    , ("Re: [VoID] a new low on the personals tip...", False)
    , ("Re: [SAtalk] SA and Patented Ideas (was: SA In The News)", False)
    ]
