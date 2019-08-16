module MOD_Dump.Module where

import Data.List
import MOD_Dump.Utils
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad

data Module = Module {
        showInfo :: [String],
        showSamples :: [Range] -> [[String]],
        showOrnaments :: [Range] -> [[String]],
        showPatterns :: [Range] -> [[String]]
    }

---------------

readModule :: Get a -> (a -> Module) -> [String] -> String -> B.ByteString -> Maybe Module
readModule mGetter fMake exts ext bs = do
    guard (ext `elem` exts)
    m <- either (const Nothing) (\(_,_,x) -> Just x) $ runGetOrFail mGetter bs
    return $ fMake m

---------------
printInfo :: Module -> IO ()
printInfo m = putStrLn $ unlines $ showInfo m

printPatterns, printSamples, printOrnaments :: Module -> Int -> [Range] -> IO ()
printPatterns   m w r = printSections w $ showPatterns m r
printSamples    m w r = printSections w $ showSamples m r
printOrnaments  m w r = printSections w $ showOrnaments m r

------------------------------

printSections :: Int -> [[String]] -> IO ()
printSections width s = putStrLn $ unlines $ map showColumned $ splitOn width s

showColumned :: [[String]] -> String
showColumned [] = []
showColumned l = unlines $ showColumned' (length $ headS $ head l) l
    where
        headS :: [String] -> String
        headS [] = ""
        headS (s:ss) = s

        showColumned' w l =  let
                                 h = intercalate "   " $ map (padSRight w . headS) l
                                 t = map (drop 1) l
                                in
                                    h : if (not.null $ concat t) then showColumned' w t else []

splitOn :: Int -> [[String]] -> [[[String]]]
splitOn _ [] = []
splitOn w xs = let (h,t) = splitAt n xs in (h : splitOn w t)
    where
        n' = length $ takeWhile (<=w) $ scanl (\l s -> l + (length $ head s) + 3) 0 xs
        n = if n' == 1 then 1 else n' - 1
