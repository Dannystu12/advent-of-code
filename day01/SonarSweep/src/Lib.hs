module Lib
  ( numDepthIncreases,
    MeasurementWindow (..),
    Depth (..),
    measurementSums,
  )
where

import Data.Map (elems, empty, fromListWith, insertWith)

numDepthIncreases :: Ord a => [a] -> Int
numDepthIncreases = numDepthIncreases' 0
  where
    numDepthIncreases' acc [] = acc
    numDepthIncreases' acc [_] = acc
    numDepthIncreases' acc' (x : x' : xs') =
      if x' > x
        then numDepthIncreases' (acc' + 1) (x' : xs')
        else numDepthIncreases' (acc') (x' : xs')

data MeasurementWindow = A | B | C | D | E | F | G | H deriving (Ord, Eq)

data Depth = Depth Int [MeasurementWindow]

measurementSums :: [Depth] -> Int
measurementSums depths = numDepthIncreases $ elems windowDepthTotals
  where
    windowDepthTotals = fromListWith (+) [(w, d) | (Depth d windows) <- depths, w <- windows]