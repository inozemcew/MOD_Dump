module MOD_Dump.Module
    ( readModule, printInfo, printPatterns, printSamples, printOrnaments
    , Module, newModule, moduleExts, getData, putData, showHeader, showRow, patternSep, showSample, showOrnament, showsPosition
    , ModuleData
    ) where

import Data.List
import MOD_Dump.Utils
import MOD_Dump.Elements
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import System.FilePath (takeExtension)

data Module = AModule
    { moduleExts :: [String]
    , getData :: Get ModuleData
    , putData :: ModuleData -> Put
    , showRow :: Row -> String
    , patternSep :: String
    , showSample :: Sample -> [String]
    , showOrnament :: Ornament -> [String]
    , showsPosition :: Position -> ShowS
    }

newModule :: Module
newModule = AModule
    { moduleExts = []
    , getData = return newModuleData
    , putData = \_ -> return ()
    , showRow = show
    , patternSep = replicate 80 '-'
    , showSample = lines.show
    , showOrnament = lines.show
    , showsPosition = \p -> ('{':) . shows (positionNumber p) . ('}':)}
---------------

readModule :: [Module] -> String -> IO (Maybe (Module,ModuleData))
readModule modules fname = do
    bs <- B.readFile fname
    return $ msum [ doRead m (takeExtension fname) bs | m <- modules ]
        where
            doRead m ext bs = do
                let exts = moduleExts m
                guard (ext `elem` exts)
                md <- readModuleData m bs
                return (m,md)

readModuleData :: Module -> B.ByteString -> Maybe ModuleData
readModuleData m bs = do
    let mGetter = getData m
    either (const Nothing) (\(_,_,x) -> Just x) $ runGetOrFail mGetter bs


---------------
printInfo :: Module -> ModuleData -> IO ()
printInfo m md = putStrLn $ unlines $ showHeader md (showsPosition m)

printPatterns, printSamples, printOrnaments :: Module -> ModuleData -> Int -> [Range] -> IO ()
printPatterns   m md w rs = printSections w $ [ showPattern m p | p <- patterns md, isInRanges rs $ patternNumber p ]
    where
        showPattern m p =  padSRight (length sep) (show p) : sep : map (showRow m) (patternRows p) ++ [sep,""]
        sep = patternSep m
printSamples    m md w rs = printSections w $ [ showSample m s | s <- samples md, isInRanges rs $ sampleNumber s ]
printOrnaments  m md w rs = printSections w $ [ showOrnament m o | o <- ornaments md, isInRanges rs $ ornamentNumber o ]

showHeader :: ModuleData -> (Position -> ShowS) -> [String]
showHeader m sp =
    [ "Song type: " ++ show (mtype m)
    , "Song name: " ++ show (title m)
    , "Delay: " ++ show (delay m)
    , "Loop to: " ++ show (loopingPos m)
    , concat $ [shows (f m) s | (f,s) <- [ (length.positions," positions, ")
                                         , (length.patterns, " patterns, ")
                                         , (length.samples, " samples, ")
                                         , (length.ornaments, " ornaments.")
                                         ]
               ]
    , "", "Positions: " ++ foldr sp "" (positions m) ]

--showsPosition :: Position -> ShowS
--showsPosition p = ('{':) . shows (positionNumber p) . ('}':)

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

