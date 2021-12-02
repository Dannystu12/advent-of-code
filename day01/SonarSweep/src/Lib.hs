module Lib
  ( numDepthIncreases,
    MeasurementWindow (..),
    Depth (..),
    measurementSums,
    threeMeasurementFilterSums,
    threeMeasurementFilter,
  )
where

import Data.Map (Map, elems, empty, fromListWith, insertWith)
import GHC.Num (Num)

numDepthIncreases :: Ord a => [a] -> Int
numDepthIncreases = numDepthIncreases' 0
  where
    numDepthIncreases' acc [] = acc
    numDepthIncreases' acc [_] = acc
    numDepthIncreases' acc' (x : x' : xs') =
      if x' > x
        then numDepthIncreases' (acc' + 1) (x' : xs')
        else numDepthIncreases' (acc') (x' : xs')

data MeasurementWindow = A | B | C | D | E | F | G | H deriving (Ord, Eq, Bounded, Enum, Show)

data Depth = Depth Int [MeasurementWindow]

measurementSums :: (Ord a, Num a, Ord b) => [[b]] -> [a] -> Int
measurementSums mFilter depths = numDepthIncreases $ elems windowDepthTotals
  where
    windowDepthTotals = foldr accumulator empty $ zip [0 ..] depths

    accumulator (i, x) acc =
      foldr addDepth acc windows
      where
        i' = i `mod` length mFilter
        windows = mFilter !! i'
        addDepth window acc = insertWith (+) window x acc

threeMeasurementFilterSums :: (Ord a, Num a) => [a] -> Int
threeMeasurementFilterSums = measurementSums threeMeasurementFilter

threeMeasurementFilter :: [[MeasurementWindow]]
threeMeasurementFilter =
  [ [A],
    [A, B],
    [A .. C],
    [B .. D],
    [C .. E],
    [D .. F],
    [E .. G],
    [F .. H],
    [G, H],
    [H]
  ]