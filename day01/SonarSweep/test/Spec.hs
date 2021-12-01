{-# LANGUAGE DataKinds #-}

module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain numDepthIncreasesTests

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