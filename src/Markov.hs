{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import Prelude hiding (lookup)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Data.Map.Strict (Map, keys, fromList, lookup, lookupGE)
import System.Random (randomIO)

printMaybe :: Show a => Maybe a -> IO ()
printMaybe = maybe (return ()) print

tally :: [String] -> (Int, [(String, Int)])
tally =
    (\xs -> (sum $ map snd xs, xs))
    . map (\xs -> (head xs, length xs))
    . group
    . sort

total :: (Int, [(String, Int)]) -> String -> (String, [(String, Int, Int)])
total (n, xs) = (, map (\(s, x) -> (s, x, n)) xs)

stack :: (String, Int, Int) -> (String, Int, Int) -> (String, Int, Int)
stack (_, x, _) (s, x', n) = (s, x + x', n)

percent :: (String, Int, Int) -> (Float, String)
percent (s, x, n) = (fromIntegral x / fromIntegral n, s)

chain :: String -> Map String (Map Float String)
chain =
    fromList
    . map
        ( (\(x, xs) -> (x, (fromList . map percent . scanl1 stack) xs))
        . (uncurry total . (\xs -> (tally $ map snd xs, fst $ head xs)))
        )
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . (\xs -> zip xs $ tail xs)
    . words

demo
    :: Map String (Map Float String)
    -> String
    -> Float
    -> Maybe (Float, String)
demo xs k k' = lookup k xs >>= lookupGE k'

example :: String
example =
    "this word is something and that word something else and this is that \
        \something and what else"

main :: IO ()
main =
    putStrLn (show xs ++ "\n")
    >> putStrLn (show (keys xs) ++ "\n")
    >> (randomIO :: IO Float)
    >>= \k' -> printMaybe (demo xs "and" k')
  where
    xs = chain example
