module MOD_Dump.STC  where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List(intersperse, intercalate, groupBy)
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
    putHeader calcTables md
    putSamples $ samples md
    putPositions $ positions md
    putOrnaments $ ornaments md
    putPatterns calcTables $ patterns md
        where
        calcTables = (pos, orn, pat)
        pos = length(samples md) * 99 + 27
        orn = pos + length(positions md) * 2 + 1
        pat = orn + length(ornaments md) * 33




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

putHeader :: (Int, Int, Int) -> ModuleData -> Put
putHeader (pos, orn, pat) md = do
    putWord8 $ fromIntegral $ delay md
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
    return $ newSampleData
        { sampleDataVolume = v
        , sampleDataNoiseMask = nm
        , sampleDataToneMask = tm
        , sampleDataTone = s
        , sampleDataNoise = n
        , sampleDataEffect = (if ss == 0 && testBit b1 5 then SDEDown else SDENone)
        }

putSampleData :: SampleData -> Put
putSampleData sd = do
    putWord8 $ fromIntegral $ sampleDataVolume sd .|. (abs(sampleDataTone sd) `shiftR` 4 .&. 0xf0)
    putWord8 $ fromIntegral $ changeBit 7 (not $ sampleDataNoiseMask sd)
           $ changeBit 6 (not $ sampleDataToneMask sd)
           $ changeBit 5 (sampleDataTone sd > 0 || (sampleDataTone sd == 0 && sampleDataEffect sd == SDEDown))
           $ sampleDataNoise sd
    putWord8 $ fromIntegral $ abs(sampleDataTone sd) .&. 0xff

showSampleData :: [SampleData] -> [String]
showSampleData ss = [concat [showVolume s i |   s <- ss ] | i <- [1..15]]
                 ++ [concatMap (\s -> [' ', showMask 'T' (sampleDataToneMask s), showMask 'N' (sampleDataNoiseMask s)]) ss
                    , foldr (\x -> showChar ' ' . showsHex 2 x) "" $ map sampleDataNoise ss
                    , showTone $ take 16 ss
                    , showTone $ drop 16 ss
                    ]
                    where
                         showMask c m = if m then c else '.'
                         showVolume s i = if sampleDataVolume s >= 16 - i then "(*)" else "..."
                         showTone ss = foldr (\x -> showChar ' ' . showsSgnInt 5 x) "" $ map sampleDataTone ss


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

putPatterns :: (Int,Int,Int) -> [Pattern] -> Put
putPatterns (_,_,ofs) ps = do
    runStateT (putOffsets offs) (-1)
    putWord8 255
    puts
        where
            pch = [(patternNumber p,ch) | p <- ps, ch <- channelsFromRows $ patternRows p ]
            pchn = map fst pch
            (puts,ds) = putChannels $ map snd pch

            offs::[(Int,Int)]
            offs = evalState (calcOffsets [ (p,l) | (l,p) <- zip ds $ map fst pch]) $ ofs + length ps * 7 + 1

            calcOffsets:: [(Int,Either Int Int)] -> State Int [(Int, Int)]
            calcOffsets = mapM $ \(n,l) -> do
                s <- get
                case l of
                     Left x  -> return (n,snd $ offs !! (x-1))
                     Right x -> do
                         put (s+x)
                         return (n,s)
            putOffsets nls = forM_ nls $ \(n,l) -> do
                o <- get
                when ( o /= n) $ lift $ putWord8 $ fromIntegral n
                lift $ putWord16le $ fromIntegral l
                put n


getPattern :: Int -> Get Pattern
getPattern n = do
            chA <-getChannel
            chB <-getChannel
            chC <-getChannel
            return $ newPattern {patternNumber = n, patternRows = makeRows [chA,chB,chC] }

putChannels :: [Channel] -> (Put, [Either Int Int])
putChannels chs = foldr (\x (ap,al) -> either (\l -> (ap, Left l : al)) (\(p,l) -> (p>>ap, Right l : al)) x) (return (), []) ds
    where
        nchs = zip [1..] chs
        ds = map doPutChannel nchs
        doPutChannel (n,ch) = let f = findSame (n,ch) in if f == n then Right $ putChannel ch else Left f
        findSame (n,ch) = fst $ head $ dropWhile (\(i,c) -> c /= ch && i<n) nchs

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
        s = maybe 0 id $ noteSample n
        p = notePitch n
        o = noteOrnament n
        showsOrnEnv = if (noteEnvForm n == noEnvForm)
                         then maybe ("000"++) (\x -> ("F0"++ ) . (showsHex 1 x)) o
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
                    | x == 0x82 = getNotes $ n{noteOrnament = Just 0, noteEnvForm = noEnvForm }
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

putChannel :: Channel -> (Put, Int) -- returns Put chain and length in bytes
putChannel ch = execState (putNotes (packChannel ch) newNote) (mempty, 0)
    where
        putNotes :: [(Note,Maybe Int)] -> Note -> State (Put, Int) ()
        putNotes []          oldn = doModify (putWord8 255) 1
        putNotes ((n,mc):ns) oldn = do
            putNote n oldn mc
            putNotes ns n

        putNote :: Note -> Note -> Maybe Int -> State (Put, Int) ()
        putNote n oldn mc = do
            maybe (return ()) (\c -> doModify (putCount c) 1) mc
            when (noteSample n /= Nothing && noteSample n /= noteSample oldn)
                $ doModify (putSample n) 1
            when (noteOrnament n /= Nothing && noteOrnament n /= noteOrnament oldn)
                $ doModify (putOrnament n) 1
            when (noteEnvForm n /= noEnvForm)
                $ doModify (putEnv n) 2
            doModify (putPitch $ notePitch n) 1

        putPitch Pause = putWord8 128
        putPitch NoNote = putWord8 129
        putPitch n = putWord8 $ fromIntegral $ (fromEnum n) - fromEnum pitchC1

        putCount c = putWord8 $ fromIntegral c + 0xa1

        putSample n = putWord8 $ (maybe 0 fromIntegral $ noteSample n) + 0x60

        putOrnament n = putWord8 $ (maybe 0 fromIntegral $ noteOrnament n) + 0x70

        putEnv n = do
            putWord8 $ fromIntegral $ (fromEnum $ noteEnvForm n) + 0x80
            putWord8 $ fromIntegral $ noteEnvFreq n

        doModify f i = modify (\(p,l)->(p >> f, l + i))

packChannel :: Channel -> [(Note, Maybe Int)]
packChannel ch = let g = [ (head x,length  x-1) | x <- groupBy (\_ x -> x == newNote) ch]
                 in concat [(x, Just y):[(i, Nothing)| (i,_) <- ts] | ((x,y):ts) <- groupBy (\x y -> snd x == snd y) g]
