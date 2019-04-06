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
    , ("RE: The absurdities of life.", False)
    , ("RE: The absurdities of life.", False)
    , ("Re: [SAtalk] SA and Patented Ideas (was: SA In The News)", False)
    ]
