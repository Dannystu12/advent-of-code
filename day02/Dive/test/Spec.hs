module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Dive Tests"
      [getPosTests]

getPosTests =
  testGroup
    "getPosTests"
    [ testCase "No movement for no commands" $
        getPos [] @?= (0,0),
      testCase "Test up" $
        getPos [Up 3] @?= (0, -3),
      testCase "Test down" $
        getPos [Down 3] @?= (0, 3),
      testCase "Test forward" $
        getPos [Forward 5] @?= (5, 0),
      testCase "Multiple commands" $
        getPos [
            Forward 5,
            Down 5,
            Forward 8,
            Up 3,
            Down 8,
            Forward 2
        ] @?= (15, 10)
    ]
