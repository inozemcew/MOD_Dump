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

trimL :: String -> String
trimL = dropWhile (==' ')

trimR :: String -> String
trimR s = foldr (\x a -> if x==' ' && null a then "" else x:a) "" s

trim :: String -> String
trim = trimR.trimL

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

---------------

lookupDef :: (Eq a) =>  b -> [(a, b)] -> a -> b
lookupDef d xs x = maybe d id $ lookup x xs

lookupChar :: (Eq a) => Char -> [(a, Char)] -> a -> ShowS
lookupChar d xs x = maybe (d:) showChar $ lookup x xs

readValue :: String -> Int
readValue = readValueDef (-1)

readValueDef :: Int -> String -> Int
readValueDef d s = let v = reads s in if null v then d else fst $ head v

whileM :: Monad m => (a -> Bool) -> m a -> m[a]
whileM c m = do
    x <- m
    if (c x) then fmap (x:) $ whileM c m
             else return []
