module Lib
  ( getPowerConsumption,
    getGammaAndEpsilon,
    BinaryNum,
    stringToBn,
  )
where

import Data.Char (digitToInt)

newtype BinaryNum = BinaryNum [Int] deriving (Show)

stringToBn :: String -> Maybe BinaryNum
stringToBn s =
  if isValid
    then Just $ BinaryNum $ map digitToInt s
    else Nothing
  where
    isValid = all (`elem` "10") s

getPowerConsumption :: [BinaryNum] -> Int
getPowerConsumption readings =
  let (gamma, epsilon) = getGammaAndEpsilon readings
   in gamma * epsilon

getGammaAndEpsilon :: [BinaryNum] -> (Int, Int)
getGammaAndEpsilon = undefined