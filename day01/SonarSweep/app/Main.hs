module Main where

import Control.Monad
import Lib (numDepthIncreases)
import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs.txt" ReadMode
  contents <- hGetContents handle

  let depths = readInts $ lines contents

  putStrLn $ "Number of depth increases: " ++ (show . numDepthIncreases $ depths)


  where
    readInts :: [String] -> [Int]
    readInts = map read