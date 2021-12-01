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
        measurementSums [] @?= 0,
      testCase "No measurement sum singleton list" $
        measurementSums [d1]
          @?= 0,
      testCase
        "Basic increase F -> G and Equality check on H"
        $ measurementSums [d8, d9]
          @?= 1,
      testCase
        "Basic decrease A -> B"
        $ measurementSums [d1, d2]
          @?= 0,
      testCase
        "Sample dataset"
        $ measurementSums [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] @?= 5
    ]
  where
    d1 = Depth 199 [A]
    d2 = Depth 200 [A, B]
    d3 = Depth 208 [A, B, C]
    d4 = Depth 210 [B, C, D]
    d5 = Depth 200 [E, C, D]
    d6 = Depth 207 [E, F, D]
    d7 = Depth 240 [E, F, G]
    d8 = Depth 269 [F, G, H]
    d9 = Depth 260 [G, H]
    d10 = Depth 263 [H]

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