module Main (main) where

import Test.HUnit
import System.Exit
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Put
import Data.Binary.Get

import MOD_Dump.Elements
import MOD_Dump.Utils
import MOD_Dump.Module
import qualified MOD_Dump.STC as STC
import qualified MOD_Dump.ASC as ASC
import qualified MOD_Dump.PT2 as PT2
import qualified MOD_Dump.PT3 as PT3

testElements = TestLabel "Elements" $ test
    [ TestCase(assertEqual "Low note" "<C--1>" (show (minBound::Pitch)))
    , TestCase(assertEqual "Hi note" "<B-10>" (show (maxBound::Pitch)))
    , TestCase(assertBool "NewInstrument " $ (isEmptyInstrument $ newInstrument))
    , TestCase(assertBool "NewInstrument with Number" $ isEmptyInstrument $ newInstrument {instrumentNumber = 5})
    , TestCase(assertBool "ModifiedInstrument " $ not $ isEmptyInstrument $ newInstrument {instrumentLoopStart = 5})
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
testPadSRight = TestCase $ assertEqual "padSRight" (padSRight 5 "1") "1    "
testPadSLeft = TestCase $ assertEqual "padSLeft" (padSLeft 5 "1") "    1"
testTrimR = TestCase $ assertEqual "trimR" (trimR "1 2    ") "1 2"

testUtils = test
            [ testShowsP
            , testShowsHex
            , testShowsSgnInt
            , testPadSLeft
            , testPadSRight
            , testTrimR
            ]

--------------------
stcFNames = [ "Bulba.stc", "AC-DC.stc" ]

testSTC = test $ testSTCSampleData : testSTCModules

testSTCSampleData = TestCase $ when (a /= b)  $ assertFailure $ unlines $ hexDiff a b : STC.showSampleData [s]
    where
        a = B.pack "\x0f\x0a0\x00"
        s = runGet STC.getSampleData $ a
        b = runPut $ STC.putSampleData s

testSTCModules = map (testModule STC.stcModule) stcFNames

--------------------
ascFName = "SKY_SURF.asc"

testASC = test
            [ testASCModule
            ]

testASCModule = testModule ASC.ascModule ascFName

--------------------
pt2FNames = ["MEGAHERZ.pt2","Grave 3.0.pt2"]

testPT2 = test $ map testPT2Module pt2FNames ++
            [ testInstrumentData pt2SampleDataNotes pt2SampleDataBytes
            , testInstrumentData pt2OrnamentDataNotes pt2OrnamentDataBytes
            ]

pt2SampleDataBytes = map B.pack
    $  [ c:"\x00\x00" | c <- "\x00\x01\x02\x03\x78\x79\x7a\x7b\xf8\xf9\xfa\xfb" ]
    ++ [ c:"\x0f\xff" | c <- "\x04\x05\x06\x07\x7c\x7d\x7e\x7f\xfc\xfd\xfe\xff" ]
    ++ [ c:"\x0f\xff" | c <- "\x00\x01\x02\x03\x78\x79\x7a\x7b\xf8\xf9\xfa\xfb" ]
    ++ [ c:"\xf0\x00" | c <- "\x00\x01\x02\x03\x78\x79\x7a\x7b\xf8\xf9\xfa\xfb" ]
    ++ [ c:"\xff\xff" | c <- "\x04\x05\x06\x07\x7c\x7d\x7e\x7f\xfc\xfd\xfe\xff" ]
    ++ [ c:"\xff\xff" | c <- "\x00\x01\x02\x03\x78\x79\x7a\x7b\xf8\xf9\xfa\xfb" ]
pt2SampleDataNotes = [ newSampleData
                        { sampleDataVolume = v
                        , sampleDataToneEnable = te
                        , sampleDataTone = t
                        , sampleDataNoiseEnable = ne
                        , sampleDataNoise = n
                        } | v <- [0, 15]
                          , t <- [0, -0xfff, 0xfff]
                          , n <- [0, 15, 31]
                          , te <- [True, False]
                          , ne <- [True, False]
                ]

pt2OrnamentDataBytes = map B.pack [[c] | c<- ['\x00'..'\xff'] ]
pt2OrnamentDataNotes = [ newOrnamentData { ornamentDataTone = x} | x <- [0..127] ++ [(-128)..(-1)] ]

testInstrumentData notes bytes = TestCase $ mapM_ doTest $ zip notes bytes
    where
        doTest (n,b) = do
            let a = runPut $ PT2.putInstrumentData [n]
            let m = runGet PT2.getInstrumentData b
            when (a /= b) $ assertFailure $ unlines $ "Wrong put" : hexDiff a b : PT2.showInstrumentData [n]
            when (m /= n) $ assertFailure $ unlines $ "Wrong get" : hexDiff a b : PT2.showInstrumentData [n,m]

testPT2Module = testModule PT2.pt2Module

---------------------

pt3FNames = ["0718.3.3.pt3", "4_liznad.3.6.pt3"]

testPT3 = test $ map testPT3Module pt3FNames

testPT3Module = testModule PT3.pt3Module

---------------------
testModule tm fName = TestCase $ do
    f <- B.readFile $ "mods/" ++ fName
    Just (m, md) <- readModule [tm] $ "mods/" ++ fName
    let b = runPut (putData m $ md)
    let a = (B.take (B.length b) f)
    when (a /= b)  $ assertFailure $ fName ++ " - module test failed\n" ++ hexDiff a b

hexDiff :: B.ByteString -> B.ByteString -> String
hexDiff a b = hexDiff' 0 a b ++ "\nErrors count: " ++ (show $ length $ filter (\(x,y) -> x/=y) $ B.zip a b)
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
    runTestTTAndExit ( TestList [testElements, testUtils, testSTC, testASC, testPT2, testPT3] )
