module MOD_Dump.STC (stcModule) where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Binary.Put
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
    , putData = putSTCModule
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

putSTCModule :: ModuleData -> Put
putSTCModule md = do
    putHeader md
    putSamples $ samples md
    putPositions $ positions md
    putOrnaments $ ornaments md
    putPatterns $ patterns md


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

putHeader :: ModuleData -> Put
putHeader md = do
    putWord8 $ fromIntegral $ delay md
    let pos = length(samples md) * 99 + 27
    let orn = pos + length(positions md) * 2 + 1
    let pat = orn + length(ornaments md) * 33
    putWord16le $ fromIntegral $ pos
    putWord16le $ fromIntegral $ orn
    putWord16le $ fromIntegral $ pat
    putLazyByteString $ B.pack $ title md
    putWord16le $ fromIntegral $ size md

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

putOrnaments :: [Ornament] -> Put
putOrnaments orns = forM_ orns $ \o -> do
    putWord8 $ fromIntegral $ ornamentNumber o
    mapM_ (putInt8.fromIntegral.ornamentDataTone) $ ornamentData o

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

putSamples :: [Sample] -> Put
putSamples ss = forM_ ss $ \s -> do
    putWord8 $ fromIntegral $ sampleNumber s
    forM_ (sampleData s ) putSampleData
    putWord8 $ fromIntegral $ sampleLoopStart s
    putWord8 $ fromIntegral $ sampleLoopEnd s - sampleLoopStart s

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
    let ss = fromIntegral (b0 .&. 0xf0) * 16 + fromIntegral b2
    let s = if testBit b1 5 then ss else -ss
    let n = fromIntegral $ b1 .&. 31
    return $ newSampleData {
        sampleDataVolume = v,
        sampleDataNoiseMask = nm,
        sampleDataToneMask = tm,
        sampleDataTone = s,
        sampleDataNoise = n
    }

putSampleData :: SampleData -> Put
putSampleData sd = do
    putWord8 $ fromIntegral $ sampleDataVolume sd .|. (abs(sampleDataTone sd) `shiftR` 4 .&. 0xf0)
    putWord8 $ fromIntegral $ changeBit 7 (not $ sampleDataNoiseMask sd)
           $ changeBit 6 (not $ sampleDataToneMask sd)
           $ changeBit 5 (sampleDataTone sd > 0)
           $ sampleDataNoise sd
    putWord8 $ fromIntegral $ abs(sampleDataTone sd) .&. 0xff

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
    replicateM (fromIntegral n + 1) $ do
        pn <- getWord8
        t <- getWord8
        return $ newPosition { positionNumber = fromIntegral pn, positionTranspose =  fromIntegral t }

putPositions :: [Position] -> Put
putPositions ps = do
    putWord8 $ fromIntegral $ length ps - 1
    forM_ ps $ \p -> do
        putWord8 $ fromIntegral $ positionNumber p
        putWord8 $ fromIntegral $ positionTranspose p


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

putPatterns :: [Pattern] -> Put
putPatterns ps = do
    mapM_ putPattern ps
    putWord8 255


getPattern :: Int -> Get Pattern
getPattern n = do
            chA <-getChannel
            chB <-getChannel
            chC <-getChannel
            return $ newPattern {patternNumber = n, patternRows = makeRows [chA,chB,chC] }

putPattern :: Pattern -> Put
putPattern p = mapM_ putChannel $ channelsFromRows $ patternRows p

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

putChannel :: Channel -> Put
putChannel ch = evalStateT (putChannel' ch) (newNote, 255, 0)
putChannel' [] = lift $ putWord8 255
putChannel' (n:ch) = if n == newNote then modify $ \(note, step, count) -> (note, step, count+1) else do
    putIf noteSample   (\x v -> x{noteSample = v})    $ \mx -> when (mx /= Nothing) $ let (Just x) = mx in putWord8 $ fromIntegral x + 0x60
    putIf noteOrnament (\x v -> x{noteOrnament = v})  $ \mx -> when (mx /= Nothing) $ let (Just x) = mx in putWord8 $ fromIntegral x + 0x70
    putIf noteEnvForm  (\x v -> x{noteEnvForm = v})   $ \x -> putWord8 $ fromIntegral $ fromEnum x + 0x80
    (oldn, step , count) <- get
    when (count /= step) $ lift $ putWord8 $ fromIntegral count + 0xa1
    put (oldn,count,0)


        where
            putIf field modF putF = do
                (oldn, step , count) <- get
                let newv = field n
                when (field oldn /= newv) $ do
                    lift $ putF newv
                    let newn = modF oldn newv in put (newn, step, count)


packChannel :: Channel -> [(Note,Maybe Int)]
packChannel cnl = doPack 0 cnl
    where
        doPack _ [] =[]
        doPack c (n:ns) = let
                              (h,t) = span (== newNote) ns
                              l = length h
                          in (n, if l == c then Nothing else Just l):doPack l t
