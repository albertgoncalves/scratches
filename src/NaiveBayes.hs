{-# OPTIONS_GHC -Wall #-}

module NaiveBayes where

import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.List (group, sort)

data Label = Label (Int, Int, String) deriving (Show)
data Proba = Proba (Float, Float, String) deriving (Eq, Show)

mapTuple :: (b -> a) -> (b, b) -> (a, a)
mapTuple = uncurry . on (,)

instance Eq Label where
    Label (_, _, c) == Label (_, _, c') = c == c'

instance Ord Label where
    Label (_, _, c) `compare` Label (_, _, c') = compare c c'

instance Semigroup Label where
    Label (a, b, c) <> Label (a', b', _) = Label (a + a', b + b', c)

instance Monoid Label where
    mempty = Label (0, 0, mempty)
    mappend = (<>)

tokenize :: String -> [String]
tokenize =
    filter (/= "")
    . map (filter isAlphaNum)
    . words
    . map toLower

tally :: Bool -> String -> Label
tally True = Label . (,,) 1 0
tally False = Label . (,,) 0 1

pipeline :: [(String, Bool)] -> [Label]
pipeline =
    map (foldl1 mappend)
    . group
    . sort
    . concat
    . map (\(xs, b) -> map (tally b) (tokenize xs))

toProba :: Float -> Label -> Label -> Proba
toProba k (Label (aTrue, aFalse, _)) (Label (bTrue, bFalse, b)) =
    Proba (pTrue, pFalse, b)
  where
    ((aTrue', aFalse'), (bTrue', bFalse')) =
        mapTuple (mapTuple fromIntegral) ((aTrue, aFalse), (bTrue, bFalse))
    k' = 2 * k
    pTrue = (bTrue' + k) / (aTrue' + k')
    pFalse = (bFalse' + k) / (aFalse' + k')

mapProba :: Float -> [Label] -> [Proba]
mapProba k xs = map (toProba k total) xs
  where
    total = foldl mappend (Label (0, 0, "[TOTALS]")) xs

classify :: String -> [Proba] -> Float
classify = undefined

examples :: [(String, Bool)]
examples =
    [ ("Friend, Copy ANY DVD or Playstation Game with this software...", True)
    , ("5% Guaranteed for Eight Years", True)
    , ("RE: The absurdities of life.", False)
    , ("RE: The absurdities of life.", False)
    , ("Re: [SAtalk] SA and Patented Ideas (was: SA In The News)", False)
    ]
