{-# OPTIONS_GHC -Wall #-}

module NaiveBayes where

import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.List (group, partition, sort)

mapTuple :: (b -> a) -> (b, b) -> (a, a)
mapTuple = uncurry . on (,)

{- / -}

newtype Label = Label (Int, Int, String) deriving (Show)

instance Eq Label where
    Label (_, _, c) == Label (_, _, c') = c == c'

instance Ord Label where
    Label (_, _, c) `compare` Label (_, _, c') = compare c c'

instance Semigroup Label where
    Label (a, b, _) <> Label (a', b', c') = Label (a + a', b + b', c')

instance Monoid Label where
    mempty = Label (0, 0, mempty)
    mappend = (<>)

{- / -}

newtype Proba = Proba (Float, Float, String) deriving (Show)

instance Eq Proba where
    Proba (_, _, c) == Proba (_, _, c') = c == c'

instance Semigroup Proba where
    Proba (a, b, c) <> Proba (a', b', _) = Proba(a + log a', b + log b', c)

instance Monoid Proba where
    mempty = Proba (0, 0, mempty)
    mappend = (<>)

instance Num Proba where
    Proba (a, b, c) + Proba (a', b', _) = Proba (a + a', b + b', c)
    Proba (a, b, c) - Proba (a', b', _) = Proba (a - a', b - b', c)
    Proba (a, b, c) * Proba (a', b', _) = Proba (a * a', b * b', c)
    abs (Proba (a, b, c)) = Proba (abs a, abs b, c)
    signum (Proba (a, b, c)) = Proba (signum a, signum b, c)
    fromInteger = fromInteger

{- / -}

tokenize :: String -> [String]
tokenize =
    filter (/= "")
    . map (filter $ \x -> isAlphaNum x || x == '\'')
    . words
    . map toLower

tally :: Bool -> String -> Label
tally True = Label . (,,) 1 0
tally False = Label . (,,) 0 1

mapLabels :: [(String, Bool)] -> [Label]
mapLabels =
    map (foldl mappend mempty)
    . group
    . sort
    . concatMap (\(xs, b) -> map (tally b) (tokenize xs))

toProba :: Float -> (Int, Int) -> Label -> Proba
toProba k totals (Label (xTrue, xFalse, x)) = Proba (pTrue, pFalse, x)
  where
    ((totalTrue', totalFalse'), (xTrue', xFalse')) =
        mapTuple (mapTuple fromIntegral) (totals, (xTrue, xFalse))
    k' = 2 * k
    pTrue = (xTrue' + k) / (totalTrue' + k')
    pFalse = (xFalse' + k) / (totalFalse' + k')

train :: Float -> [(String, Bool)] -> [Proba]
train k xs = map (toProba k (totalTrue, totalFalse)) $ mapLabels xs
  where
    totalTrue = length $ filter snd xs
    totalFalse = length xs - totalTrue

classify :: [Proba] -> String -> (String, Float)
classify ys x = (x, p / (p + q))
  where
    xs = map (Proba . (,,) 0 0) $ tokenize x
    (ps, qs) = partition (`elem` xs) ys
    (ps', qs') =
        mapTuple (foldl mappend mempty) (ps, map (Proba (1, 1, mempty) -) qs)
    Proba (pTrue, pFalse, _) = ps' + qs'
    (p, q) = mapTuple exp (pTrue, pFalse)

{- / -}

corpus :: [(String, Bool)]
corpus =
    [ ("5% Guaranteed for Eight Years", True)
    , ("Congratulations! You Get a Free Handheld Organizer!", True)
    , ("One of a kind Money maker! Try it for free!", True)
    , ("Take your Marketing to the Next Level", True)
    , ("One Sale - Three Commission Streams", True)
    , ("Find Peace, Harmony, Tranquility, And Happiness Right Now!", True)
    , ("ADV: Extended Auto Warranties Here undoc", True)
    , ("Definitely the answer many have been waiting for!!", True)
    , ("Save $100's, maybe $1,000's with No Lender's Fees. Click here!", True)
    , ("Get the Computer Skills you need - Free", True)
    , ("[WM] CEVIRI YAZILIMLARI", True)
    , ("Let me know what you think!32482", True)
    , ("Garden Ornaments | ppu", True)
    , ("A marketplace where lenders compete for your business LFHLXHU", True)
    , ("Cheap Fags", True)
    , ("Hey, rates are low. What are you waiting for?", True)
    , ("One of a kind Money maker! Try it for free!", True)
    , ("[ILUG-Social] Lose 22.5lbs in 3 weeks!", True)
    , ("Re: The case for spam", False)
    , ("Re: traceback in new exmh", False)
    , ("Re: The GOv gets tough on Net Users.....er Pirates..", False)
    , ("Gold Lake Mountain Resort looks pretty gooood. Man there are a", False)
    , ("Sheila Lennon was interviewed for the Times piece.", False)
    , ("Re: Entrepreneurs", False)
    , ("Re: bad focus/click behaviours", False)
    , ("[Spambayes] test sets?", False)
    , ("Hopes fade in Ulster crisis talks", False)
    , ("UFOs in the Sky!", False)
    , ("rpm-zzzlist@freshrpms.net", False)
    , ("[Spambayes] spambayes package?", False)
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

exTrue :: String
exTrue = "Online Doctors will fill your Viagra Prescription Now!!! QEEB"

exFalse :: String
exFalse = "RE: Microsoft buys XDegress - more of a p2p/distributed data thing"

{- / -}

main :: IO ()
main = mapM_ print train' >> mapM_ (print . classify train') xs
  where
    train' = train 0.5 corpus
    xs = [exTrue, exFalse]
