{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Data.Map.Strict (fromList, lookup, lookupGE, Map)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)
import System.Random (random)
import System.Random.TF (seedTFGen, TFGen)

head' :: Monoid a => [a] -> a
head' [] = mempty
head' (x:_) = x

{- / -}

tokenize :: String -> [String]
tokenize =
    filter (/= "")
    . map (filter $ \x -> isAlphaNum x || x == '\'')
    . words
    . map toLower

tally :: [String] -> ([(String, Int)], Int)
tally =
    (\xs -> (xs, sum $ map snd xs))
    . map (\xs -> (head' xs, length xs))
    . group
    . sort

accumulate :: (String, Int, Int) -> (String, Int, Int) -> (String, Int, Int)
accumulate (_, n, _) (x, n', m) = (x, n + n', m)

toFraction :: (String, Int, Int) -> (Float, String)
toFraction (x, n, m) = (fromIntegral n / fromIntegral m, x)

encode :: [(String, Int, Int)] -> Map Float String
encode =
    fromList
    . map toFraction
    . scanl1 accumulate

transformPair
    :: [((String, String), String)]
    -> ((String, String), Map Float String)
transformPair xs = (x, encode $ map f xs')
  where
    (xs', m) = tally (map snd xs)
    x = fst (head' xs)
    f (x', n) = (x', n, m)

chain :: [String] -> Map (String, String) (Map Float String)
chain =
    fromList
    . map transformPair
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . map (\(a, b, c) -> ((a, b), c))
    . (\xs -> zip3 xs (drop 1 xs) (drop 2 xs))

generate
    :: Map (String, String) (Map Float String)
    -> Maybe ((String, String), TFGen)
    -> Maybe ((String, String), TFGen)
generate _ Nothing = Nothing
generate xs (Just (k, g)) =
    (, g') . (snd k, ) . snd
    <$> (lookup k xs >>= lookupGE k')
  where
    (k', g') = random g

writeSentence
    :: Int
    -> (Maybe ((String, String), a) -> Maybe ((String, String), a))
    -> Maybe ((String, String), a)
    -> String
writeSentence n f =
    unwords
    . map (fst . fst)
    . catMaybes
    . take n
    . iterate f

{- / -}

example :: String
example = "Still less must this kind of contentment, which holds science in contempt, take upon itself to claim that raving obscurantism of this sort is something higher than science. These apocalyptic utterances pretend to occupy the very centre and the deepest depths; they look askance at all definiteness and preciseness of meaning; and they deliberately hold back from conceptual thinking and the constraining necessities of thought, as being the sort of reflection which, they say, can only feel at home in the sphere of finitude. But just as there is a breadth which is emptiness, there is a depth which is empty too: as we may have an extension of substance which overflows into finite multiplicity without the power of keeping the manifold together, in the same way we may have an insubstantial intensity which, keeping itself in as mere force without actual expression, is no better than superficiality. The force of mind is only as great as its expression; its depth only as deep as its power to expand and lose itself when spending and giving out its substance. Moreover, when this unreflective emotional knowledge makes a pretence of having immersed its own very self in the depths of the absolute Being, and of philosophizing in all holiness and truth, it hides from itself the fact that instead of devotion to God, it rather, by this contempt for all measurable precision and definiteness, simply attests in its own case the fortuitous character of its content, and in the other endows God with its own caprice. When such minds commit themselves to the unrestrained ferment of sheer emotion, they think that, by putting a veil over self-consciousness, and surrendering all understanding, they are thus God's beloved ones to whom He gives His wisdom in sleep. This is the reason, too, that in point of fact, what they do conceive and bring forth in sleep is dreams."

{- / -}

main :: IO ()
main = (print . writeSentence n f) seed
  where
    seed = Just (("we", "may"), seedTFGen (0, 0, 0, 0))
    f = (generate . chain . tokenize) example
    n = 25
