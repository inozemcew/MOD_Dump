module MOD_Dump.STC  where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import MOD_Dump.FlagSet
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List(intersperse, intercalate, groupBy, mapAccumL, mapAccumR)
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

    let samplesCount = (minimum [f tables | f <- [positionsTable, ornamentsTable, patternsTable]] - 27) `div` 99
    samples'    <- getSamples samplesCount

    positions'  <- lookAhead $ skipTo (positionsTable tables) >> getPositions

    let ornamentsCount = (patternsTable tables - ornamentsTable tables) `div` 33
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
    putHeader calcTables md
    putSamples $ samples md
    putPositions $ positions md
    putOrnaments $ ornaments md
    putPatterns calcTables $ patterns md
        where
            calcTables = newTables { positionsTable = pos, ornamentsTable = orn, patternsTable = pat }
            pos = length(samples md) * 99 + 27
            orn = pos + length(positions md) * 2 + 1
            pat = orn + length(ornaments md) * 33



getHeader :: Get (Int, Tables, String, Int)
getHeader = do
    delay   <- getAsWord8
    tables  <- getTables
    identifier  <- B.unpack <$> getLazyByteString 18
    size    <- getAsWord16le
    return (delay, tables, identifier, size)
        where
            getTables = do
                pos <- getAsWord16le
                orn <- getAsWord16le
                pat <- getAsWord16le
                return $ newTables
                    { positionsTable = pos
                    , ornamentsTable = orn
                    , patternsTable = pat
                    }

putHeader :: Tables -> ModuleData -> Put
putHeader ts md = do
    putAsWord8 $ delay md
    putAsWord16le $ positionsTable ts
    putAsWord16le $ ornamentsTable ts
    putAsWord16le $ patternsTable ts
    putLazyByteString $ B.pack $ title md
    putAsWord16le $ size md

-----------------------------------------------------------------------------

getOrnaments :: Int -> Get [Ornament]
getOrnaments ornamentsCount = replicateM ornamentsCount getOrnament
    where
        getOrnament = do
            l <- getAsWord8
            ds <- replicateM 32 $ do
                x <- getAsInt8
                return newOrnamentData { ornamentDataTone = x }
            return $ newOrnament { ornamentNumber = l, ornamentData = ds }

putOrnaments :: [Ornament] -> Put
putOrnaments orns = forM_ orns $ \o -> do
    putAsWord8 $ ornamentNumber o
    mapM_ (putAsInt8.ornamentDataTone) $ ornamentData o

showSTCOrnament :: Ornament -> [String]
showSTCOrnament orn = let o = concat [showsSgnInt 3 (ornamentDataTone d) " " |  d <- ornamentData orn]
                      in [padSRight (length o) $ "Ornament: " ++ (show $ ornamentNumber orn), o]

-----------------------------------------------------------------------------

getSamples :: Int -> Get [Sample]
getSamples samplesCount = replicateM samplesCount getSample
    where
        getSample = do
            n <- getAsWord8
            d <- replicateM 32 getSampleData
            p <- getAsWord8
            l <- getAsWord8
            return $ newSample {
                sampleNumber = n,
                sampleData = d,
                sampleLoopStart = p,
                sampleLoopEnd = p + l
            }

putSamples :: [Sample] -> Put
putSamples ss = forM_ ss $ \s -> do
    putAsWord8 $ sampleNumber s
    forM_ (sampleData s ) putSampleData
    putAsWord8 $ sampleLoopStart s
    putAsWord8 $ sampleLoopEnd s - sampleLoopStart s

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
    [b0, b1, b2] <- replicateM 3 getWord8
    let v = fromIntegral $ b0 .&. 15
    let nm = not $ testBit b1 7
    let tm = not $ testBit b1 6
    let ss = fromIntegral (b0 .&. 0xf0) * 16 + fromIntegral b2
    let s = if testBit b1 5 then ss else if ss == 0 then -0x10000 else -ss
    let n = fromIntegral $ b1 .&. 31
    return $ newSampleData
        { sampleDataVolume = v
        , sampleDataNoiseEnable = nm
        , sampleDataToneEnable = tm
        , sampleDataTone = s
        , sampleDataNoise = n
        }

putSampleData :: SampleData -> Put
putSampleData sd = do
    putAsWord8 $ sampleDataVolume sd .|. (abs(sampleDataTone sd) `shiftR` 4 .&. 0xf0)
    putAsWord8 $ changeBit 7 (not $ sampleDataNoiseEnable sd)
           $ changeBit 6 (not $ sampleDataToneEnable sd)
           $ changeBit 5 (sampleDataTone sd >= 0)
           $ sampleDataNoise sd
    putAsWord8 $ abs(sampleDataTone sd) .&. 0xff

showSampleData :: [SampleData] -> [String]
showSampleData ss =
    [concat [showVolume s i |   s <- ss ] | i <- [1..15]] ++
        [concatMap (\s -> [' ', showMask 'T' (sampleDataToneEnable s), showMask 'N' (sampleDataNoiseEnable s)]) ss
        , foldr (\x -> showChar ' ' . showsHex 2 x) "" $ map sampleDataNoise ss
        , showTone $ take 16 ss
        , showTone $ drop 16 ss
        ]
    where
        showMask c m = if m then c else '.'
        showVolume s i = if sampleDataVolume s >= 16 - i then "(*)" else "..."
        showTone ss = foldr (\x -> (' ':) . showsSgnInt 5 x) "" $ [0xfff .&. sampleDataTone s | s <- ss]


-----------------------------------------------------------------------------

getPositions :: Get [Position]
getPositions = do
    n <- getAsWord8
    replicateM (n + 1) $ do
        pn <- getAsWord8
        t <- getAsWord8
        return $ newPosition { positionNumber = pn, positionTranspose = t }

putPositions :: [Position] -> Put
putPositions ps = do
    putAsWord8 $ length ps - 1
    forM_ ps $ \p -> do
        putAsWord8 $ positionNumber p
        putAsWord8 $ positionTranspose p


----------------------------------------------------------------------------

getPatterns :: Get [Pattern]
getPatterns = do
    n <- getAsWord8
    if n == 255
        then return []
        else do
            p <- getPattern n
            ps <- getPatterns
            return $ p:ps

putPatterns :: Tables -> [Pattern] -> Put
putPatterns ts ps = do
    foldM_ putOffsets (-1) $ zip pts offs
    putWord8 255
    puts
        where
            (pts, chs) = unzip [(patternNumber p, ch) | p <- ps, ch <- channelsFromRows $ patternRows p ]
            (puts, ds) = putChannels chs

            offs :: [Int] -- Channels_offsets
            offs =  snd $ mapAccumL calcOffset (patternsTable ts + length ps * 7 + 1) ds

            calcOffset s x = either (\l -> (s, offs !! (l-1))) (\r -> (r + s, s)) x

            putOffsets o (n, l) = do
                when ( o /= n) $ putAsWord8 n
                putAsWord16le l
                return n


getPattern :: Int -> Get Pattern
getPattern n = do
            chs <- replicateM 3 getChannel
            return $ newPattern { patternNumber = n, patternRows = makeRows chs }

putChannels :: [Channel] -> (Put, [Either Int Int])
putChannels chs = mapAccumR doPutChannel (return ()) nchs
    where
        nchs = zip [1..] chs
        findSame (n, ch) = fst $ head $ dropWhile (\(i,c) -> c /= ch && i < n) nchs
        doPutChannel ap (n, ch) = let f = findSame (n, ch)
                                  in if f == n
                                        then let (p, l) = putChannel ch
                                             in (p >> ap, Right l)
                                        else (ap, Left f)

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
showsNote n = if (isPitch p) then showsPitch p . (' ':) . showsHex 1 s . showsOrnEnv
    else
        showsPitch p . showString " ----"
    where
        s = noteSample n
        p = notePitch n
        o = noteOrnament n
        showsOrnEnv = if (noteEnvForm n == noEnvForm)
                         then if o == 0 then ("000"++) else ("F0"++ ) . (showsHex 1 o)
                         --maybe ("000"++) (\x -> ("F0"++ ) . (showsHex 1 x)) o
                         else showsHex 1 (fromEnum $ noteEnvForm n) . showsHex 2 (noteEnvFreq n)


getChannel :: Get [Note]
getChannel = do
    s <- getAsWord16le
    lookAhead $ do
        skipTo s
        evalStateT getNewNotes 0

    where
        getNotes:: Note -> StateT Int Get [Note]
        getNotes n = do
            v <- lift getAsWord8
            switch v
            where
                switch x
                    | x == 255 = return []
                    | x <= 0x5f = yieldNote $ n{notePitch = toEnum $ x + fromEnum pitchC1}
                    | x == 0x80 = yieldNote $ n{notePitch = Pause}
                    | x == 0x81 = yieldNote $ n{notePitch = Release}
                    | x == 0x82 = getNotes $ n{noteOrnament = 0, noteEnvForm = noEnvForm }
                    | x >= 0x60 && x <= 0x6f = getNotes $ n{ noteSample = (x - 0x60)
                                                           , noteFlags = noteFlags n `set` ChangedSample }
                    | x >= 0x70 && x <= 0x7f = getNotes $ n{ noteOrnament = (x - 0x70)
                                                           , noteFlags = noteFlags n `set` ChangedOrnament }
                    | x >= 0x83 && x <= 0x8e = do
                        o <- lift getAsWord8
                        getNotes $ n{ noteEnvForm = toEnum (x - 0x80), noteEnvFreq = o
                                    , noteFlags = noteFlags n `set` ChangedEnvForm `set` ChangedEnvFreq}
                    | x >= 0xa1 && x <= 0xfe = do
                        put (x - 0xa1)
                        getNotes n
                    | otherwise = getNotes n

                yieldNote x = do
                    r <- get
                    ns <- getNotes n{notePitch = NoNote, noteFlags = noFlags}
                    return $ x : replicate r newNote ++ ns

        getNewNotes = getNotes newNote

putChannel :: Channel -> (Put, Int) -- returns Put chain and length in bytes
putChannel ch = (p >> putWord8 255, l + 1)
    where
        (p, l) = execState (foldM_ putNote newNote (packChannel ch)) (mempty, 0)

        putNote oldn (n, mCnt) = do
            maybe (return ()) (\c -> doModify (putCount c) 1) mCnt
            when (noteFlags n `isSet` ChangedSample)   $ doModify (putSample n) 1
            when (noteFlags n `isSet` ChangedOrnament) $ doModify (putOrnament n) 1
            let (form, freq) = (noteEnvForm n, noteEnvFreq n) in
                when (noteFlags n `anySet` [ChangedEnvForm, ChangedEnvFreq]) $ doModify (putEnv form freq) 2
            doModify (putPitch $ notePitch n) 1
            return n

        putPitch Pause   = putWord8 128
        putPitch Release = putWord8 129
        putPitch NoNote  = return ()
        putPitch n       = putAsWord8 $ (fromEnum n) - fromEnum pitchC1

        putCount c = putAsWord8 $ c + 0xa1

        putSample n = putAsWord8 $ noteSample n + 0x60

        putOrnament n = putAsWord8 $ noteOrnament n + 0x70

        putEnv form freq  = do
            putAsWord8 $ fromEnum form + 0x80
            putAsWord8 freq

        doModify f i = modify (\(p,l)->(p >> f, l + i))

