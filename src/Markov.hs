{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import Prelude hiding (lookup)
import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Data.Map.Strict (Map, keys, fromList, lookup, lookupGE)
import System.Random (randomIO)

{- / -}

printEndline :: Show a => a -> IO ()
printEndline x = putStrLn (show x ++ "\n")

printMaybe :: Show a => Maybe a -> IO ()
printMaybe = maybe (return ()) print

{- / -}

tally :: [String] -> (Int, [(String, Int)])
tally =
    (\xs -> (sum $ map snd xs, xs))
    . map (\xs -> (head xs, length xs))
    . group
    . sort

arrange :: (Int, [(String, Int)]) -> String -> (String, [(String, Int, Int)])
arrange (n, xs) = (, map (\(s, x) -> (s, x, n)) xs)

cumsum :: (String, Int, Int) -> (String, Int, Int) -> (String, Int, Int)
cumsum (_, x, _) (s, x', n) = (s, x + x', n)

percent :: (String, Int, Int) -> (Float, String)
percent (s, x, n) = (fromIntegral x / fromIntegral n, s)

pairsToDict :: [(String, String)] -> (String, Map Float String)
pairsToDict =
    (\(x, xs) -> (x, (fromList . map percent . scanl1 cumsum) xs))
    .  uncurry arrange
    . (\xs -> (tally $ map snd xs, fst $ head xs))

tokenize :: String -> [String]
tokenize =
    filter (/= "")
    . map (filter (\x -> isAlphaNum x || x == '\''))
    . words
    . map toLower

chain :: [String] -> Map String (Map Float String)
chain =
    fromList
    . map pairsToDict
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . (\xs -> zip xs $ tail xs)

{- / -}

demo
    :: Map String (Map Float String)
    -> String
    -> Float
    -> Maybe (Float, String)
demo xs k k' = lookup k xs >>= lookupGE k'

example :: String
example =
    "This word is something and that word is something else. And, also, this \
        \is that something and what else."

{- / -}

main :: IO ()
main =
    printEndline xs
    >> printEndline (keys xs)
    >> (randomIO :: IO Float)
    >>= \k' -> printMaybe (demo xs "and" k')
  where
    xs = (chain . tokenize) example
