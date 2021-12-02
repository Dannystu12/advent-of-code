{-# LANGUAGE DataKinds #-}

module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "SonarSweep Tests"
      [numDepthIncreasesTests, measurementSumsTests]

measurementSumsTests =
  testGroup
    "measurementSumsTests"
    [ testCase "No measurement sum for empty list" $
        threeMeasurementFilterSums [] @?= 0,
      testCase "No measurement sum singleton list" $
        threeMeasurementFilterSums [d1]
          @?= 0,
      testCase
        "Basic decrease A -> B"
        $ threeMeasurementFilterSums [d1, d2]
          @?= 0,
      testCase
        "Sample dataset"
        $ threeMeasurementFilterSums [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] @?= 5
    ]
  where
    d1 = 199 :: Int
    d2 = 200 :: Int
    d3 = 208 :: Int
    d4 = 210 :: Int
    d5 = 200 :: Int
    d6 = 207 :: Int
    d7 = 240 :: Int
    d8 = 269 :: Int
    d9 = 260 :: Int
    d10 = 263 :: Int

numDepthIncreasesTests =
  testGroup
    "numDepthIncreasesTests"
    [ testCase "No depth increases for empty list" $
        numDepthIncreases
          ( [] ::
              [Int]
          )
          @?= 0,
      testCase "No depth increases for singleton list" $
        numDepthIncreases
          ( [] ::
              [Int]
          )
          @?= 0,
      testCase "No depth increase for list of equal depths" $
        numDepthIncreases [1, 1]
          @?= 0,
      testCase
        "One depth increase for ascending 2 element list"
        $ numDepthIncreases [1, 2]
          @?= 1,
      testCase
        "No depth increase for descending 2 element list"
        $ numDepthIncreases [2, 1] @?= 0,
      testCase "Basic case" $
        numDepthIncreases
          [ 199,
            200,
            208,
            210,
            200,
            207,
            240,
            269,
            260,
            263
          ]
          @?= 7
    ]