{-# LANGUAGE BlockArguments #-}

module Lib
  ( numDepthIncreases,
    measurementSums,
    threeMeasurementSums,
  )
where

numDepthIncreases :: Ord a => [a] -> Int
numDepthIncreases = numDepthIncreases' 0
  where
    numDepthIncreases' acc [] = acc
    numDepthIncreases' acc [_] = acc
    numDepthIncreases' acc' (x : x' : xs') =
      if x' > x
        then numDepthIncreases' (acc' + 1) (x' : xs')
        else numDepthIncreases' acc' (x' : xs')

measurementSums :: (Ord a, Num p, Num a) => Int -> [a] -> p
measurementSums _ [] = 0
measurementSums n xs
  | length xs < n + 1 = next
  | x < y = 1 + next
  | otherwise = next
  where
    x = sum $ take n xs
    y = sum . tail $ take (n + 1) xs
    next = measurementSums n $ tail xs

threeMeasurementSums :: [Int] -> Int
threeMeasurementSums = measurementSums 3