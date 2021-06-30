module Main (main) where

import Test.HUnit
import System.Exit
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Put

import MOD_Dump.Elements
import MOD_Dump.Utils
import MOD_Dump.Module
import MOD_Dump.STC


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

testSTC = TestCase $ do
    f <- B.readFile "mods/AC-DC.stc"
    Just (m, md) <- readModule [stcModule] "mods/AC-DC.stc"
    let b = runPut (putData m $ md)
    let a = (B.take (B.length b) f)
    when (a /= b)  $ assertFailure $ hexDiff a b

hexDiff :: B.ByteString -> B.ByteString -> String
hexDiff a b = hexDiff' 0 a b
    where
        hdr n = if (n `mod` 16 == 0) then ('\n':) .showsHex 4 n. (' ':) else (' ':)
        hexDiff' n a b = if B.null a || B.null b
                        then ""
                        else hdr n
                            .showsHex 2 (fromEnum $ B.head a)
                            .(':':)
                            .showsHex 2 (fromEnum $ B.head b)
                            .(if (B.head a == B.head b) then (' ':) else ('<':))
                            $ hexDiff' (n+1) (B.tail a) (B.tail b)

main :: IO ()
main = do
    runTestTTAndExit ( TestList [testElements, testUtils, testSTC] )
