module Main where

import Control.Monad (forM_, replicateM)
import Data.List (intercalate, transpose)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.Random
import Text.Printf (printf)

main :: IO ()
main = do
    let maxRounds = 10000
    probs1 <- randomFractionalLists maxRounds 4
    probs2 <- randomFractionalLists maxRounds 6
    probs3 <- randomFractionalLists maxRounds 8

    let p = [x * 0.05 | x <- [0 .. 20]]
        k1 = [1, 2, 4] :: [Int]
        k2 = [1, 3, 6] :: [Int]
        k3 = [1, 4, 8] :: [Int]

    let resultsK1 = map (\k -> map (\pValue -> simulation k pValue probs1) p) k1
    let resultsK2 = map (\k -> map (\pValue -> simulation k pValue probs2) p) k2
    let resultsK3 = map (\k -> map (\pValue -> simulation k pValue probs3) p) k3

    savePlot "proporcoes_k1.png" "k1 = [1, 2, 4]" k1 resultsK1 p
    savePlot "proporcoes_k2.png" "k2 = [1, 3, 6]" k2 resultsK2 p
    savePlot "proporcoes_k3.png" "k3 = [1, 4, 8]" k3 resultsK3 p

    saveTable "tabela_k1.csv" k1 resultsK1 p
    saveTable "tabela_k2.csv" k2 resultsK2 p
    saveTable "tabela_k3.csv" k3 resultsK3 p

    putStrLn "Gráficos gerados: proporcoes_k1.png, proporcoes_k2.png, proporcoes_k3.png"
    putStrLn "Tabelas geradas: tabela_k1.csv, tabela_k2.csv, tabela_k3.csv"

savePlot :: FilePath -> String -> [Int] -> [[Double]] -> [Double] -> IO ()
savePlot filename title kValues results pValues =
    toFile def filename $ do
        layout_title .= title
        layout_x_axis . laxis_title .= "Valores de p"
        layout_y_axis . laxis_title .= "Proporção Experimental"

        forM_ (zip kValues results) $ \(k, res) ->
            plot (line ("k=" ++ show k) [zip pValues res])

simulation :: Int -> Double -> [[Double]] -> Double
simulation k p probs = fromIntegral successfullRounds / fromIntegral maxRounds
  where
    maxRounds = length probs
    allRounds = map (\i -> [ii <= p | ii <- i]) probs
    resultsRounds = [length (filter id r) >= k | r <- allRounds]
    successfullRounds = length $ filter id resultsRounds

randomFractionalLists :: Int -> Int -> IO [[Double]]
randomFractionalLists n m = replicateM n (replicateM m randomIO)

saveTable :: FilePath -> [Int] -> [[Double]] -> [Double] -> IO ()
saveTable filename kValues results pValues = do
    let header = "p," ++ intercalate "," (map (\k -> "k=" ++ show k) kValues)
    let transposed = transpose results
    let rows =
            zipWith
                ( \pVal vs ->
                    printf "%.2f" pVal ++ "," ++ intercalate "," (map (printf "%.4f") vs)
                )
                pValues
                transposed
    let content = unlines (header : rows)
    writeFile filename content
