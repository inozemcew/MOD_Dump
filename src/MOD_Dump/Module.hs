module MOD_Dump.Module where

import qualified Data.ByteString.Lazy.Char8 as B

type Range = (Int, Int)

data Module = Module {
        printInfo :: IO (),
        printSamples :: [Range] -> IO (),
        printOrnaments :: [Range] -> IO (),
        printPatterns :: Int -> [Range] -> IO ()
    }

filterInRange :: (a -> Int) -> [Range] -> [a] -> [a]
filterInRange _ [] xs = xs
filterInRange f r xs = [ x| x <- xs , let i = f x, any (\(f,t) -> i>=f && i<=t) r]

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs = let (h,t) = splitAt n xs in h : splitBy n t

