{-# LANGUAGE LambdaCase #-}

module Lib
  ( getPowerConsumption,
    getGammaAndEpsilon,
    BinaryNum,
    stringToBn,
    getGamma,
    getEpsilon,
    bnToInt,
    transpose,
    unwrap
  )
where

import Data.Char (digitToInt)
import Data.List (foldl')

newtype BinaryNum = BinaryNum [Int] deriving (Show)

instance Eq BinaryNum where
  (==) a b = bnToInt a == bnToInt b


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
getGammaAndEpsilon readings = (bnToInt gamma, bnToInt epsilon)
  where
    gamma = getGamma readings
    epsilon = getEpsilon gamma

getGamma :: [BinaryNum] -> BinaryNum
getGamma nums = BinaryNum $ foldl accum [] $ transpose $ map unwrap nums
  where
    accum acc xs =
        if zeros >= majorityThreshold' then
            acc ++ [0]
        else
            acc ++ [1]
        where
            majorityThreshold' = if odd totalLen then  majorityThreshold + 1 else majorityThreshold
            majorityThreshold = totalLen `div` 2
            totalLen = length xs
            zeros = length $ filter (==0) xs 

getEpsilon :: BinaryNum -> BinaryNum
getEpsilon (BinaryNum n) =
  BinaryNum $
    map
      ( \case
          1 -> 0
          0 -> 1
          _ -> error "Invalid binary num"
      )
      n

bnToInt :: BinaryNum -> Int
bnToInt (BinaryNum bn) = foldl (\acc x -> acc * 2 + x) 0 bn

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose x = (map head x) : transpose (map tail x)

unwrap :: BinaryNum -> [Int]
unwrap (BinaryNum bn) = bn