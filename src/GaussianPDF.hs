{-# OPTIONS_GHC -Wall #-}

import Text.Printf (printf)

printMaybe :: Maybe String -> IO ()
printMaybe = maybe (return ()) putStrLn

showFloat :: Float -> String
showFloat = printf "%.8f"

mean :: (Integral a, Floating b) => a -> [b] -> b
mean n xs = sum xs / fromIntegral n

std :: Floating a => Int -> [a] -> Maybe a
std _ [] = Nothing
std _ [_] = Nothing
std d xs = (Just . sqrt . mean (n - d) . map f) xs
  where
    n = length xs
    f = (** 2) . (mean n xs -)

gaussianPDF :: Floating a => a -> a -> a -> a
gaussianPDF mu sigma x = (1 / denom) * expon
  where
    sigma' = sigma ** 2
    expon = (exp . negate) $ ((x - mu) ** 2) / (sigma' * 2)
    denom = sqrt $ 2 * pi * sigma'

{-
 -  $ R
 -  > xs = ...
 -  > dnorm(xs, mean(xs), sd(xs))
 -}
mapGP :: Floating a => [a] -> Maybe [a]
mapGP xs = f <$> std 1 xs
  where
    n = length xs
    mu = mean n xs
    f sigma = map (gaussianPDF mu $ sigma) xs

main :: IO ()
main = f xs
  where
    f = printMaybe . (unlines <$>) . (map showFloat <$>) . mapGP
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
