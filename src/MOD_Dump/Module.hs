module MOD_Dump.Module
    ( readModule, printInfo, printPatterns, printSamples, printOrnaments
    , Module, newModule, moduleExts, getData, showHeader, showRow, patternSep, showSample, showOrnament
    , ShowModule, newShowModule, showInfo, showSamples, showOrnaments, showPatterns
    ) where

import Data.List
import MOD_Dump.Utils
import MOD_Dump.Elements
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad

data ShowModule = AShowModule
    { showInfo :: [String]
    , showSamples :: [Range] -> [[String]]
    , showOrnaments :: [Range] -> [[String]]
    , showPatterns :: [Range] -> [[String]]
    }

newShowModule :: Module -> ModuleData -> ShowModule
newShowModule m md = AShowModule --{ showInfo = [], showSamples = const [], showOrnaments = const [], showPatterns = const [] }
    { showInfo =  showHeader m md
    , showPatterns  = \rs -> [ showPattern m p | p <- patterns md, isInRanges rs $ patternNumber p ]
    , showSamples   = \rs -> [ showSample m s | s <- samples md, isInRanges rs $ sampleNumber s ]
    , showOrnaments = \rs -> [ showOrnament m o | o <- ornaments md, isInRanges rs $ ornamentNumber o ] }
        where
            showPattern m p =  padSRight (length sep) (show p) : sep : map (showRow m) (patternRows p) ++ [sep,""]
            sep = patternSep m

data Module = AModule
    { moduleExts :: [String]
    , getData :: Get ModuleData
    , showHeader :: ModuleData -> [String]
    , showRow :: Row -> String
    , patternSep :: String
    , showSample :: Sample -> [String]
    , showOrnament :: Ornament -> [String] }

newModule :: Module
newModule = AModule
    { moduleExts = []
    , getData = return newModuleData
    , showHeader = lines.show
    , showRow = show -- const ["No patterns"]
    , patternSep = replicate 80 '-'
    , showSample = lines.show
    , showOrnament = lines.show }
---------------

readModule :: Module -> String -> B.ByteString -> Maybe ShowModule
readModule m ext bs = do
    let exts = moduleExts m
    guard (ext `elem` exts)
    let mGetter = getData m
    md <- either (const Nothing) (\(_,_,x) -> Just x) $ runGetOrFail mGetter bs
    return $ newShowModule m md

---------------
printInfo :: ShowModule -> IO ()
printInfo sm = putStrLn $ unlines $ showInfo sm

printPatterns, printSamples, printOrnaments :: ShowModule -> Int -> [Range] -> IO ()
printPatterns   sm w rs = printSections w $ showPatterns sm rs
printSamples    sm w rs = printSections w $ showSamples sm rs
printOrnaments  sm w rs = printSections w $ showOrnaments sm rs

------------------------------

printSections :: Int -> [[String]] -> IO ()
printSections width s = putStrLn $ unlines $ map showColumned $ splitOn width s

showColumned :: [[String]] -> String
showColumned [] = []
showColumned l = unlines $ showColumned' l
    where

        ws = map (length . headS) l

        headS :: [String] -> String
        headS [] = ""
        headS (s:ss) = s

        showColumned' l =  let
                                 h = intercalate "   " $ zipWith (\s w -> padSRight w $ headS s) l ws
                                 t = map (drop 1) l
                           in
                                 h : if (not.null $ concat t) then showColumned' t else []

splitOn :: Int -> [[String]] -> [[[String]]]
splitOn _ [] = []
splitOn w xs = let (h,t) = splitAt n xs in (h : splitOn w t)
    where
        n' = length $ takeWhile (<=w) $ scanl (\l s -> l + (length $ head s) + 3) 0 xs
        n = if n' == 1 then 1 else n' - 1

-------------------------------

