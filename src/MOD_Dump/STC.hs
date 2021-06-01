module MOD_Dump.STC (stcModule) where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Bits
import Data.List(intersperse, intercalate)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State


stcModule :: Module
stcModule = newModule
    { moduleExts = [".stc"]
    , getData = getSTCModule
    , showRow = showSTCRow
    , patternSep = "---+----------+----------+---------"
    , showSample = showSTCSample
    , showOrnament = showSTCOrnament
    , showsPosition = \p -> ('{':) . shows (positionNumber p) . if positionTranspose p /=0 then showsSgnInt 2 (positionTranspose p) else id . ('}':)

    }

getSTCModule :: Get ModuleData
getSTCModule = do
    (delay', tables, identifier', size') <- getHeader

    let samplesCount = fromIntegral (minimum [f tables | f <- [positionsTable, ornamentsTable, patternsTable]] - 27) `div` 99
    samples'    <- getSamples samplesCount

    positions'  <- lookAhead $ skipTo (positionsTable tables) >> getPositions

    let ornamentsCount = fromIntegral ((patternsTable tables - ornamentsTable tables) `div` 33)
    ornaments'  <- lookAhead $ skipTo (ornamentsTable tables) >> getOrnaments ornamentsCount

    patterns'   <- skipTo (patternsTable tables) >> getPatterns

    return newModuleData
        { mtype = "Sound Tracker compiled song"
        , delay = delay'
        , positions = positions'
        , ornaments = ornaments'
        , patterns = patterns'
        , title = identifier'
        , size = size'
        , samples = samples'
        }

getHeader :: Get (Int, Tables, String, Int)
getHeader = do
    delay   <- fromIntegral `liftM` getWord8
    tables  <- getTables
    identifier  <- B.unpack `liftM` getLazyByteString 18
    size    <- fromIntegral `liftM` getWord16le
    return (delay, tables, identifier, size)
        where
            getTables = do
                pos <- getWord16le
                orn <- getWord16le
                pat <- getWord16le
                return $ newTables
                    { positionsTable = fromIntegral pos
                    , ornamentsTable = fromIntegral orn
                    , patternsTable = fromIntegral pat
                    }

-----------------------------------------------------------------------------

getOrnaments :: Int -> Get [Ornament]
getOrnaments ornamentsCount = replicateM ornamentsCount getOrnament
    where
        getOrnament = do
            l <- fromIntegral `liftM` getWord8
            ds <- replicateM 32 $ do
                x <- getInt8
                return newOrnamentData { ornamentDataTone = fromIntegral x }
            return $ newOrnament { ornamentNumber = l, ornamentData = ds }

showSTCOrnament :: Ornament -> [String]
showSTCOrnament orn = let o = concat [showsSgnInt 3 (ornamentDataTone d) " " |  d <- ornamentData orn]
                   in [padSRight (length o) $ "Ornament: " ++ (show $ ornamentNumber orn), o]

-----------------------------------------------------------------------------

getSamples :: Int -> Get [Sample]
getSamples samplesCount = replicateM samplesCount getSample
    where
        getSample = do
            n <- fromIntegral `liftM` getWord8
            d <- replicateM 32 getSampleData
            p <- fromIntegral `liftM` getWord8
            l <- fromIntegral `liftM` getWord8
            return $ newSample {
                sampleNumber = n,
                sampleData = d,
                sampleLoopStart = p,
                sampleLoopEnd = p + l
            }

showSTCSample :: Sample -> [String]
showSTCSample s = (padSRight width32 $ "Sample: " ++ show (sampleNumber s))
    : replicate width32 '-'
    : showSampleData (sampleData s)
    ++ [footer start end]
        where
            start = sampleLoopStart s
            end = sampleLoopEnd s
            width = 3
            width32 = width * 32
            footer 0 _ = replicate (width*32) '-'
            footer s e = if e > 32 then out (e - 32) (32 - e + s) (32 - s) '#' '-'
                                   else out (s - 1) (e-s) (33 - e) '-' '#'
            out n1 n2 n3 c1 c2 = replicate (width*n1) c1 ++ replicate (width*n2) c2 ++ replicate (width*n3) c1

---------------
getSampleData :: Get SampleData
getSampleData = do
    b0 <- getWord8
    b1 <- getWord8
    b2 <- getWord8
    let v = fromIntegral $ b0 .&. 15
    let nm = not $ testBit b1 7
    let tm = not $ testBit b1 6
    let ss = fromIntegral $ shiftR b0 4 .|. b2
    let s = if testBit b1 5 then ss else -ss
    let n = fromIntegral $ b1 .&. 31
    return $ newSampleData {
        sampleDataVolume = v,
        sampleDataNoiseMask = nm,
        sampleDataToneMask = tm,
        sampleDataTone = s,
        sampleDataNoise = n
    }

showSampleData :: [SampleData] -> [String]
showSampleData ss = [concat [showVolume s i |   s <- ss ] | i <- [1..15]]
                 ++ [concatMap (\s -> [' ', showMask 'T' (sampleDataToneMask s), showMask 'N' (sampleDataNoiseMask s)]) ss]
                 ++ [foldr (\x -> showChar ' ' . showsHex 2 x) "" $ map sampleDataNoise ss]
                    where
                         showMask c m = if m then c else '.'
                         showVolume s i = if sampleDataVolume s >= 16 - i then "(*)" else "..."


-----------------------------------------------------------------------------

getPositions :: Get [Position]
getPositions = do
    n <- getWord8
    replicateM (fromIntegral n) $ do
        pn <- getWord8
        t <- getWord8
        return $ newPosition { positionNumber = fromIntegral pn, positionTranspose =  fromIntegral t }

----------------------------------------------------------------------------

getPatterns :: Get [Pattern]
getPatterns = do
    n <- fromIntegral `liftM` getWord8
    if n == 255
        then return []
        else do
            p <- getPattern $ fromIntegral n
            ps <- getPatterns
            return $ p:ps

getPattern :: Int -> Get Pattern
getPattern n = do
            chA <-getChannel
            chB <-getChannel
            chC <-getChannel
            return $ newPattern {patternNumber = n, patternRows = makeRows [chA,chB,chC] }


showSTCRow :: Row -> String
showSTCRow r = foldr id "" $ intersperse (" | " ++) $ showsHex 2 (rowNumber r) : ( map showsNote $ rowNotes r )

-----------------------------------------------------------------------------

showsPitch Pause = showString "R--"
showsPitch (Pitch n o) =  showString ( noteNames !! fromEnum n) . shows (o)
showsPitch _ = showString "---"

noteNames :: [String]
noteNames = ["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"]

-----------------------------------------------------------------------------

showsNote :: Note -> ShowS
showsNote n = if (isPitch p) then showsPitch p . (' ':) . showsHex 1 (maybe 0 id $ noteSample n) . showsOrnEnv
    else
        showsPitch p . showString " ----"
    where
        p = notePitch n
        o = noteOrnament n
        showsOrnEnv = if (noteEnvForm n == EnvFormNone)
                         then ((if o == Nothing then "00" else "F0") ++ ) . showsHex 1 (maybe 0 id o)
                         else showsHex 1 (fromEnum $ noteEnvForm n) . showsHex 2 (noteEnvFreq n)


getChannel :: Get [Note]
getChannel = do
    s <- fromIntegral `liftM` getWord16le
    lookAhead $ do
        skipTo s
        evalStateT getNewNotes 0

    where
        getNotes:: Note -> StateT Int Get [Note]
        getNotes n = do
            v <- lift getWord8
            switch $ fromIntegral v
            where
                switch x
                    | x == 255 = return []
                    | x <= 0x5f = yieldNote $ n{notePitch = toEnum $ x + fromEnum pitchC1}
                    | x == 0x80 = yieldNote $ n{notePitch = Pause}
                    | x == 0x81 = yieldNote $ n{notePitch = NoNote}
                    | x == 0x82 = getNotes $ n{noteOrnament = Just 0, noteEnvForm = EnvFormNone }
                    | x >= 0x60 && x <= 0x6f = getNotes $ n{ noteSample = Just (x - 0x60) }
                    | x >= 0x70 && x <= 0x7f = getNotes $ n{ noteOrnament = Just (x - 0x70) }
                    | x >= 0x83 && x <= 0x8e = do
                        o <- lift getWord8
                        getNotes $ n{noteEnvForm = toEnum (x - 0x80), noteEnvFreq = fromIntegral o}
                    | x >= 0xa1 && x <= 0xfe = do
                        put (x - 0xa1)
                        getNotes n
                    | otherwise = getNotes n

        getNewNotes = getNotes newNote

        yieldNote x = do
            r <- get
            ns <- getNewNotes
            return $ x : replicate r newNote ++ ns

-----------------------------------------------------------------------------
