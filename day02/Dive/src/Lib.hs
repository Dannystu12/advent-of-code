module Lib
  ( getPos,
    Command (..),
  )
where

import Data.Char (digitToInt, toLower)
import Data.List (intersperse)
import Data.Maybe (fromJust, isNothing)
import Numeric (readInt)
import Text.Read (readMaybe)

data Command = Forward Int | Down Int | Up Int deriving (Show)

instance Read Command where
  readsPrec _ input =
    let (direction : magnitude : rest) = words input
        d = case map toLower direction of
          "forward" -> Just Forward
          "down" -> Just Down
          "up" -> Just Up
          _ -> Nothing
        m = readMaybe magnitude :: Maybe Int
        validParse = not $ isNothing d || isNothing m
        result
          | isNothing d || isNothing m = Forward 0
          | otherwise = fromJust d $ fromJust m
     in [(result, unwords rest)]

getPos :: [Command] -> (Int, Int)
getPos =
  foldl accum (0, 0)
  where
    accum (hpos, depth) (Forward x) = (hpos + x, depth)
    accum (hpos, depth) (Up x) = (hpos, depth - x)
    accum (hpos, depth) (Down x) = (hpos, depth + x)
