{-# OPTIONS_GHC -Wall #-}

import Text.Printf (printf)

showFloat :: Float -> String
showFloat = printf "%.8f"

mean :: (Integral a, Floating b) => a -> [b] -> Maybe b
mean _ [] = Nothing
mean 0 _ = Nothing
mean n xs = Just (sum xs / fromIntegral n)

std :: Floating a => Int -> [a] -> Maybe a
std _ [] = Nothing
std _ [_] = Nothing
std d xs =
    mean n xs
    >>= \mean' -> mean (n - d) (map (f mean') xs)
    >>= Just . sqrt
  where
    n = length xs
    f x = (** 2) . (x -)

gaussianPDF :: (Eq a, Floating a) => a -> a -> a -> Maybe a
gaussianPDF _ 0 _ = Nothing
gaussianPDF mu sigma x = Just $ (1 / denom) * expon
  where
    sigma' = sigma ** 2
    expon = (exp . negate) $ ((x - mu) ** 2) / (sigma' * 2)
    denom = sqrt (2 * pi * sigma')

{-
 -  $ R
 -  > xs = ...
 -  > dnorm(xs, mean(xs), sd(xs))
 -}

autoGPDF:: (Eq a, Floating a) => [a] -> Maybe [a]
autoGPDF xs =
    mean (length xs) xs
    >>= \mu -> std 1 xs
    >>= \sigma -> mapM (gaussianPDF mu sigma) xs

pipeline :: [Float] -> IO ()
pipeline =
    putStrLn
    . maybe "Nothing" (unlines . map showFloat)
    . autoGPDF

main :: IO ()
main = pipeline xs
  where
    xs =
        [ -0.9924723
        , -0.1592349
        , -0.1538372
        , 1.5335746
        , 0.3457412
        , -1.5293901
        , -0.4949839
        , 0.1531161
        , -1.2940066
        , -0.9109157
        ] :: [Float]
