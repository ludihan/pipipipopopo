module Main where

main :: IO ()
main = do
    let
        n = 10
        p = 0.8
        k = 7 :: Integer
        maxRounds = 10000

        probs = replicate maxRounds (replicate n 0.9)

        proporcao = simulation k p probs :: Double
    putStrLn $ "Proporção experimental: " ++ show proporcao

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

binomCoef :: (Integral a, Fractional b) => a -> a -> b
binomCoef n k = fromIntegral (factorial n) / fromIntegral (factorial k * factorial (n - k))

binomDist :: (Integral a, Fractional b) => a -> a -> b -> b
binomDist n k p = product [binomCoef n k, p ^ k, (1 - p) ^ (n - k)]

simulation :: (Integral a, Fractional b, Ord b) => a -> b -> [[b]] -> b
simulation k p probs = fromIntegral successfullRounds / fromIntegral maxRounds
  where
    maxRounds = length probs
    allRounds = map (\i -> [ii <= p | ii <- i]) probs
    resultsRounds = [length (filter id r) >= fromIntegral k | r <- allRounds]
    successfullRounds = length $ filter id resultsRounds
