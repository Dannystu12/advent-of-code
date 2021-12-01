module Lib
  ( numDepthIncreases,
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
        else numDepthIncreases' (acc') (x' : xs')
