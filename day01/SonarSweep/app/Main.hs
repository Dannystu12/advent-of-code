module Main where

import Control.Monad
import Lib (numDepthIncreases, threeMeasurementSums)
import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs.txt" ReadMode
  contents <- hGetContents handle

  let depths = readInts $ lines contents

  putStrLn $ "Number of depth increases: " ++ (show . numDepthIncreases $ depths)
  putStrLn $ "Number of sliding window increases: " ++ (show . threeMeasurementSums $ depths)
  where
    readInts :: [String] -> [Int]
    readInts = map read