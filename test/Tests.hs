module Main (main) where

import Test.HUnit
import System.Exit
import Control.Monad

import MOD_Dump.Elements
import MOD_Dump.Utils

testElements = TestLabel "Elements" $ test
                [ TestCase(assertEqual "Low note" "<C--1>" (show (minBound::Pitch)))
                , TestCase(assertEqual "Hi note" "<B-10>" (show (maxBound::Pitch)))
                ]


--------------------

testShowsP = TestCase $ forM_ [(1,"xx1"),(22,"x22"),(333,"333"),(4444,"4444")] $
                \(i,e) -> assertEqual "ShowP 3"  e (showsP 'x' 3 i $ "")
testShowsHex = TestCase $ forM_ [(1,2,"01"),(127,2,"7F"),(32768,4,"8000")] $
                \(i,w,h) -> assertEqual "Showhex"  h (showsHex w i $ "")
testShowsSgnInt = TestCase $ forM_ [-10..10] $
                \i -> assertBool "ShowsSgnInt" ( let s = showsSgnInt 3 i $ "" in
                                                     length s == 3 && if i<0
                                                                         then head s == '-'
                                                                         else head s == '+')
testUtils = test
            [ testShowsP
            , testShowsHex
            , testShowsSgnInt
            ]


--------------------

main :: IO ()
main = do
    runTestTTAndExit ( TestList [testElements, testUtils] )
