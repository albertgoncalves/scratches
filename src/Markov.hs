{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Data.Map.Strict (Map, fromDistinctAscList, lookup, lookupGE)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)
import System.Random (random)
import System.Random.TF (TFGen, seedTFGen)

{- / -}

tokenize :: String -> [String]
tokenize =
    filter (/= "")
    . map (filter $ \x -> isAlphaNum x || x == '\'')
    . words
    . map toLower

tally :: [String] -> (Int, [(String, Int)])
tally =
    (\xs -> (sum $ map snd xs, xs))
    . map (\xs -> (head xs, length xs))
    . group
    . sort

arrange :: (Int, [(String, Int)]) -> String -> (String, [(String, Int, Int)])
arrange (n, xs) = (, map (\(s, x) -> (s, x, n)) xs)

accumulate :: (String, Int, Int) -> (String, Int, Int) -> (String, Int, Int)
accumulate (_, x, _) (s, x', n) = (s, x + x', n)

toFraction :: (String, Int, Int) -> (Float, String)
toFraction (s, x, n) = (fromIntegral x / fromIntegral n, s)

encode :: [(String, Int, Int)] -> Map Float String
encode =
    fromDistinctAscList
    . map toFraction
    . scanl1 accumulate

pairTransform :: [(String, String)] -> (String, Map Float String)
pairTransform =
    (\(x, xs) -> (x, encode xs))
    .  uncurry arrange
    . (\xs -> (tally $ map snd xs, fst $ head xs))

chain :: [String] -> Map String (Map Float String)
chain =
    fromDistinctAscList
    . map pairTransform
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . (\xs -> zip xs $ tail xs)

generate
    :: Map String (Map Float String)
    -> Maybe (String, TFGen)
    -> Maybe (String, TFGen)
generate _ Nothing = Nothing
generate xs (Just (k, g)) =
    lookup k xs
    >>= lookupGE k'
    >>= \(_, v) -> return (v, g')
  where
    (k', g') = random g

writeSentence
    :: Int
    -> (Maybe (String, a) -> Maybe (String, a))
    -> Maybe (String, a)
    -> String
writeSentence n f =
    unwords
    . map fst
    . catMaybes
    . take n
    . iterate f

{- / -}

example :: String
example = "Still less must this kind of contentment, which holds science in contempt, take upon itself to claim that raving obscurantism of this sort is something higher than science. These apocalyptic utterances pretend to occupy the very centre and the deepest depths; they look askance at all definiteness and preciseness of meaning; and they deliberately hold back from conceptual thinking and the constraining necessities of thought, as being the sort of reflection which, they say, can only feel at home in the sphere of finitude. But just as there is a breadth which is emptiness, there is a depth which is empty too: as we may have an extension of substance which overflows into finite multiplicity without the power of keeping the manifold together, in the same way we may have an insubstantial intensity which, keeping itself in as mere force without actual expression, is no better than superficiality. The force of mind is only as great as its expression; its depth only as deep as its power to expand and lose itself when spending and giving out its substance. Moreover, when this unreflective emotional knowledge makes a pretence of having immersed its own very self in the depths of the absolute Being, and of philosophizing in all holiness and truth, it hides from itself the fact that instead of devotion to God, it rather, by this contempt for all measurable precision and definiteness, simply attests in its own case the fortuitous character of its content, and in the other endows God with its own caprice. When such minds commit themselves to the unrestrained ferment of sheer emotion, they think that, by putting a veil over self-consciousness, and surrendering all understanding, they are thus God’s beloved ones to whom He gives His wisdom in sleep. This is the reason, too, that in point of fact, what they do conceive and bring forth in sleep is dreams."

{- / -}

main :: IO ()
main = (print . writeSentence n f) seed
  where
    seed = Just ("god", seedTFGen (0, 0, 0, 0))
    f = (generate . chain . tokenize) example
    n = 24
