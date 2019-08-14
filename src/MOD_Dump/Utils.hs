module MOD_Dump.Utils where

import Data.Binary.Get
import Control.Monad
import Control.Monad.Trans
import Data.List(intercalate)

showsP :: Char -> Int -> Int -> ShowS
showsP c n i = (padLeft c n (show i) ++)

showsZ :: Int -> Int -> ShowS
showsZ = showsP '0'

showsB :: Int -> Int -> ShowS
showsB = showsP ' '

shows2 :: Int -> ShowS
shows2 = showsZ 2

shows3 :: Int -> ShowS
shows3 = showsZ 3

showsHex :: (Integral i) => Int ->  i -> ShowS
showsHex 0 _ = showString ""
showsHex n x = let (r,q) = divMod x 16 in  showsHex (n-1) r . showChar (hexDigits !! fromIntegral q)
    where hexDigits = "0123456789ABCDEF"

shows32 :: Int -> ShowS
shows32 x = if x<10 then shows x else showChar $ toEnum (x+55)

showsSgnInt :: Int -> Int -> ShowS
showsSgnInt len n
    | n >= 0 = ('+':) . showsZ (len-1) n
    | otherwise = ('-':) . showsZ (len-1) (0-n)


padLeft :: Char -> Int -> String -> String
padLeft c n s = let l = length s in if l>=n then s else take (n-l) (repeat c) ++ s

padRight :: Char -> Int -> String -> String
padRight c n s = let l = length s in if l>=n then s else s ++ take (n-l) (repeat c)

padSLeft :: Int -> String -> String
padSLeft = padLeft ' '

padSRight :: Int -> String -> String
padSRight = padRight ' '

---------------
type Range = (Int, Int)

isInRanges :: [Range] ->  Int -> Bool
isInRanges [] _ = True
isInRanges r x = any (\(f,t) -> x>=f && x<=t) r

---------------
peekWord16 :: Int -> Get Int
peekWord16 n = lookAhead $ skip n >> fromIntegral `liftM` getWord16le

skipTo :: Int -> Get ()
skipTo n = do
    c <- bytesRead
    skip (n - fromIntegral c)
