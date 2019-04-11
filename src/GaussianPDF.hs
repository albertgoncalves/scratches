{-# OPTIONS_GHC -Wall #-}

import Text.Printf (printf)

mean :: (Integral a, Floating b) => a -> [b] -> Maybe b
mean _ [] = Nothing
mean n xs
    | n <= 0 = Nothing
    | otherwise = Just (sum xs / fromIntegral n)

std :: (Integral a, Floating b) => a -> a -> [b] -> Maybe b
std _ _ [] = Nothing
std _ _ [_] = Nothing
std n d xs = sqrt <$> (mean n xs >>= \mean' -> mean (n - d) $ map (f mean') xs)
  where
    f x = (** 2) . (x -)

gaussianPDF :: (Eq a, Floating a) => a -> a -> a -> Maybe a
gaussianPDF _ 0 _ = Nothing
gaussianPDF mu sigma x = Just $ (1 / denom) * expon
  where
    sigma' = sigma ** 2
    expon = (exp . negate) $ ((x - mu) ** 2) / (sigma' * 2)
    denom = sqrt (2 * pi * sigma')

{- $ R
   > xs = ...
   > dnorm(xs, mean(xs), sd(xs)) -}
autoGPDF:: (Eq a, Floating a) => [a] -> Maybe [a]
autoGPDF xs =
    mean n xs
    >>= \mu -> std n 1 xs
    >>= \sigma -> mapM (gaussianPDF mu sigma) xs
  where
    n = length xs

pipeline :: [Float] -> IO ()
pipeline =
    putStrLn
    . maybe "Nothing" (unlines . map f)
    . autoGPDF
  where
    f = printf "%.8f"

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
        ]
