module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Dive Tests"
      [getPosTests, getPosTests']

getPosTests =
  testGroup
    "getPosTests"
    [ testCase "No movement for no commands" $
        getPos [] @?= (0, 0),
      testCase "Test up" $
        getPos [Up 3] @?= (0, -3),
      testCase "Test down" $
        getPos [Down 3] @?= (0, 3),
      testCase "Test forward" $
        getPos [Forward 5] @?= (5, 0),
      testCase "Multiple commands" $
        getPos
          [ Forward 5,
            Down 5,
            Forward 8,
            Up 3,
            Down 8,
            Forward 2
          ]
          @?= (15, 10)
    ]

getPosTests' =
  testGroup
    "getPosTests'"
    [ testCase "No movement for no commands" $
        getPos' [] @?= (0, 0),
      testCase "Test up" $
        getPos' [Up 3] @?= (0, 0),
      testCase "Test down" $
        getPos' [Down 3] @?= (0, 0),
      testCase "Test forward" $
        getPos' [Forward 5] @?= (5, 0),
      testCase "Test forward after down" $
        getPos' [Down 5, Forward 5] @?= (5, 25),
      testCase "Test forward after down then up" $
        getPos' [Down 3, Up 2, Forward 5] @?= (5, 5),
      testCase "Multiple commands" $
        getPos'
          [ Forward 5,
            Down 5,
            Forward 8,
            Up 3,
            Down 8,
            Forward 2
          ]
          @?= (15, 60)
    ]
