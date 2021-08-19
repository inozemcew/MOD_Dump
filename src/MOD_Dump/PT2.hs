module MOD_Dump.PT2 where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List(mapAccumR)

pt2Module :: Module
pt2Module = newModule
    { moduleExts = [".pt2"]
    , getData = getModuleData
    , putData = putModuleData
    , showRow = showPT2Row
    , patternSep = "--+----+--------+--------+--------+ "
    , showSample = showPT2Sample
    , showOrnament = showPT2Ornament }

getModuleData :: Get ModuleData
getModuleData = do
    delay' <- getAsWord8
    len <- getAsWord8
    loopingPos' <- getAsWord8
    x <- getWord16le
    guard (x == 0)
    samples' <- getSamples
    oorn0 <- getAsWord16le
    orn0 <- lookAhead $ do
        skipTo oorn0
        liftM2 (,) getWord16le getWord8
    guard (orn0 == (1,0))
    ornaments' <- getOrnaments
    pOffset <- getAsWord16le
    title' <- B.unpack <$> getLazyByteString 30
    poss <- replicateM len $ getAsWord8
    x <- getWord8
    guard (x == 255)
    x <- bytesRead
    guard (x == pOffset)
    patterns' <- getPatterns $ maximum poss
    t <- getWord16le
    guard (t == 0)

    return newModuleData
        { mtype = "Pro Tracker 2 compiled song"
        , delay = delay'
        , loopingPos = loopingPos'
        , positions = [ newPosition { positionNumber = i } | i <- poss]
        , patterns = patterns'
        , samples = filter (\s -> not.null $ sampleData s) samples'
        , ornaments = filter (\s -> not.null $ ornamentData s) ornaments'
        , title = title' }

putModuleData :: ModuleData -> Put
putModuleData md = do
    putAsWord8 $ delay md
    putAsWord8 $ length $ positions md
    putAsWord8 $ loopingPos md
    putWord16le 0
    oo <- foldM foldOffs chStart samplesLengths
    putAsWord16le oo
    foldM_ foldOffs (oo+3) $ ornamentsLengths
    putAsWord16le patOffset
    putLazyByteString $ B.pack $ padSRight 30 $ title md
    forM_ (positions md) $ \p -> putAsWord8 $ positionNumber p
    putWord8 255
    forM_ chsOffs putAsWord16le
    putWord16le 0
    doPutPatterns
    doPutSamples
    putWord8 1
    putWord16le 0
    doPutOrnaments

        where
            (doPutSamples, samplesLengths) = putSamples $ samples md
            (doPutOrnaments, ornamentsLengths) = putOrnaments $ ornaments md
            patOffset = 132 + length (positions md)
            (doPutPatterns, channelsLengths) = putPatterns $ patterns md
            (chsOffs, chStart) = runState calcOffsets $ 2 * length channelsLengths + 2 + patOffset

            foldOffs s o = if o==0 then putWord16le 0 >> return s
                                   else do
                                       putAsWord16le $ s
                                       return $ s + o
            calcOffsets = forM channelsLengths $ \l -> case l of
                                                            Left x  -> return $ chsOffs !! (x-1)
                                                            Right x -> state (\s -> (s, x + s))



-------------------------------

showPT2Row :: Row -> String
showPT2Row r = showsHex 2 (rowNumber r) . ('|':) . showsShared (rowShared r)
               $ (foldr (\x -> ('|':) . showsNote x) "|" (rowNotes r))

showsShared :: Shared -> ShowS
showsShared s = showsHex 4 (sharedEnvFreq s)

showsNote :: Note -> ShowS
showsNote n = showsPitch (notePitch n)
            . showsCmd (noteCmd n)
            . showsHex 1 (maybe 0 id $ noteSample n)
            . showsEForm
            . showsHex 1 (maybe 0 id $ noteOrnament n)
            . showsHex 1 (maybe 0 id $ noteVolume n)
    where
        showsPitch (Pitch k o ) = showString (["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"] !! fromEnum k) . shows o
        showsPitch Pause = showString "R--"
        showsPitch _ = showString "---"

        showsCmd NoteCmdNone = (' ':)
        showsCmd _ = ('*':)

        showsEForm = if noteEnvForm n `elem` [Nothing, Just EnvFormNone]
                        then maybe ('0':) (const ('F':)) $  noteOrnament n
                        else let (Just ef) = noteEnvForm n in showsHex 1 $ fromEnum ef


getPatterns :: Int -> Get [Pattern]
getPatterns n = forM [0..n] $ \i -> do
    here <- bytesRead
    chs <- replicateM 3 $ do
        ch <- getAsWord16le
        guard (ch > fromIntegral here)
        return ch
    channels <- mapM (lookAhead . getChannel) chs
    return newPattern
        { patternNumber = i
        , patternRows = makeRowsWithShared makeShared channels }

putPatterns :: [Pattern] -> (Put,[Either Int Int])
putPatterns pts = foldr foldChannel mempty nchs
    where
        chs = concatMap (channelsFromRows . patternRows) pts
        nchs = zip [1..] chs
        findSame (n,ch) = fst $ head $ dropWhile (\(i,c) -> c /= ch && i < n) nchs
        foldChannel (n, ch) (ap, al) = let (p, l) = putChannel ch
                                           f = findSame (n, ch)
                                       in  if f == n then (p >> ap, Right l : al)
                                                     else (ap, Left f : al)


makeShared :: [Note] -> Shared
makeShared notes = newShared { sharedEnvFreq = foldl (\a x -> if noteEnvFreq x == noEnvFreq then a else noteEnvFreq x) noEnvFreq notes }

getChannel :: Int -> Get Channel
getChannel offset = do
    skipTo offset
    evalStateT getNewNotes 0

putChannel :: [Note] -> (Put, Int)
putChannel ch = execState (foldM doPutNote newNote (packChannel ch) >> doModify1 0 ) (mempty, 0)
    where
        doPutNote oN (nN, mc) = do
            maybe (return ()) (\x -> doModify1 $ 0x20 + x) mc
            when (noteSample oN /= noteSample nN) $ doModifyM 0xe0 $ noteSample nN
            when (noteEnvForm nN /= Nothing) $ do
                let (Just ef) = fromEnum <$> noteEnvForm nN
                doModify1 $ 0x70 + ef
                when (ef <15) $ doModify 2 $ putAsWord16le $ noteEnvFreq nN
            when (noteOrnament oN /= noteOrnament nN) $ doModifyM 0x60 $ noteOrnament nN
            when (noteVolume oN /= noteVolume nN) $ doModifyM 0x10 $ noteVolume nN
            when (noteCmd nN /= NoteCmdNone) $ case noteCmd nN of
                    NoteCmdDelay n -> doModify2 0x0f n
                    NoteCmdGlis n  -> doModify2 0x0e n
                    NoteCmdPorta _ f g -> do
                        doModify2 0xd0 g
                        doModify 2 $ putAsInt16le f
                    NoteCmdNoise n -> doModify2 0x0b n
                    _ -> return ()
            doModify1 $ putPitch $ notePitch nN
            return nN

        putPitch Release = 0xe0
        putPitch Pause = 0xe0
        putPitch NoNote = 0x70
        putPitch n = fromEnum n - fromEnum pitchC1 + 0x80

        doModify x m = modify $ \(p, l) -> (p >> m, l + x)
        doModify1 x = doModify 1 $ putAsWord8 x
        doModify2 c n = doModify 2 $ putWord8 c >> putAsWord8 n
        doModifyM x mz = maybe (lift mempty) (\y -> doModify1 (x + y)) mz





getNewNotes :: StateT Int Get [Note]
getNewNotes = getNotes newNote

getNotes :: Note -> StateT Int Get [Note]
getNotes note = do
    n <- lift getAsWord8
    switch n
        where
            switch x
                | x == 0x00 = return []
                | x >= 0xe1 = getNotes $ note { noteSample = Just (x - 0xe0) }
                | x == 0xe0 = yieldNotes $ note { notePitch = Pause }
                | x >= 0x80 = yieldNotes $ note { notePitch = toEnum $ x - 0x80 + fromEnum pitchC1 }
                | x == 0x7f = getNotes $ note { noteEnvForm = noEnvForm }
                | x >= 0x71 = do
                    freq <- lift getAsWord16le
                    getNotes $ note { noteEnvForm = Just $ toEnum (x - 0x70), noteEnvFreq = freq }
                | x == 0x70 = yieldNotes note
                | x >= 0x60 = getNotes $ note { noteOrnament = Just (x - 0x60) }
                | x >= 0x20 = put (x - 0x20) >> getNotes note
                | x >= 0x10 = getNotes $ note { noteVolume = Just (x - 0x10) }
                | x == 0x0f = do
                    d <- lift getAsWord8
                    getNotes $ note { noteCmd = NoteCmdDelay d }
                | x == 0x0e = do
                    g <- lift getAsInt8
                    getNotes note { noteCmd = NoteCmdGlis g }
                | x == 0x0d = do
                    g <- lift getAsWord8
                    f <- lift getAsInt16le
                    getNotes $ note { noteCmd = NoteCmdPorta 0 f g }
                | x == 0x0c = getNotes $ note { noteCmd = NoteCmdNone }
                | x == 0x0b = do
                    g <- lift getAsWord8
                    getNotes $ note { noteCmd = NoteCmdNoise g }
                | otherwise = getNotes note

            yieldNotes note = do
                r <- get
                ns <- getNewNotes
                return $ note : replicate r newNote ++ ns


------------------------------

getInstruments :: (InstrumentData d) => Int -> Get [Instrument d]
getInstruments n = forM [1..n-1] $ \i -> do
    offset <- getAsWord16le
    if offset == 0
       then return $ newInstrument {instrumentNumber = i}
       else lookAhead $  do
            skipTo offset
            len <- getAsWord8
            loopStart <- getAsWord8
            d <- replicateM len getInstrumentData
            return $ newInstrument
                { instrumentNumber = i
                , instrumentData = d
                , instrumentLoopStart = loopStart }

putInstruments :: (InstrumentData d) => Int -> [Instrument d] -> (Put, [Int])
putInstruments n ins = mapAccumR doPutInstrument mempty expandInstruments
    where
        expandInstruments = evalState (mapM doExpand [1..n-1]) ins
        doExpand c = do
            i <- get
            let (h:t) = i
            if null i || c < instrumentNumber h
               then return Nothing
               else do
                   put t
                   return $ Just h
        doPutInstrument ap Nothing = (ap,0)
        doPutInstrument ap (Just i) = (putInstrument i >> ap, 2 + sizeInstrumentData (instrumentData i))

putInstrument :: (InstrumentData d) => Instrument d -> Put
putInstrument h = do
                    let ids = instrumentData h
                    putAsWord8 $ length ids
                    putAsWord8 $ instrumentLoopStart h
                    putInstrumentData ids

class InstrumentData d where
    getInstrumentData :: Get d
    putInstrumentData :: [d] -> Put
    sizeInstrumentData :: [d] -> Int
    showInstrumentData :: [d] -> [String]

getSamples :: Get [Sample]
getSamples = getInstruments 32

putSamples :: [Sample] -> (Put, [Int])
putSamples = putInstruments 32

instance InstrumentData SampleData where
    getInstrumentData = getSampleData
    putInstrumentData = putSampleData
    sizeInstrumentData = sizeSampleData
    showInstrumentData = showSampleData

getSampleData :: Get SampleData
getSampleData = do
    b0 <- getAsWord8
    b1 <- getAsWord8  -- bits 7-4 = volume, bits 3-0 tone high nibble
    b2 <- getAsWord8  -- tone low byte
    let noiseEnable = not $ b0 `testBit` 0
    let toneEnable  = not $ b0 `testBit` 1
    let tone' = (b2 + (b1 `shiftL` 8)) .&. 0xfff
    let tone = if b0 `testBit` 2 then if tone' == 0 then -0x1000 else negate tone' else tone'
    let noise = b0 `shiftR` 3
    let volume = b1 `shiftR` 4

    return newSampleData
                { sampleDataNoiseEnable = noiseEnable
                , sampleDataToneEnable = toneEnable
                , sampleDataTone = tone
                , sampleDataNoise = noise
                , sampleDataVolume = volume }

putSampleData :: [SampleData] -> Put
putSampleData sds = mapM_ doPutSampleData sds
    where
        doPutSampleData sd= do
            putWord8
                $ changeBit 0 (not $ sampleDataNoiseEnable sd)
                $ changeBit 1 (not $ sampleDataToneEnable sd)
                $ changeBit 2 (sampleDataTone sd < 0)
                $ fromIntegral $ sampleDataNoise sd `shiftL` 3
            let sdt = if sampleDataTone sd >= 0 then sampleDataTone sd else negate $ sampleDataTone sd
            putAsWord8 $ (sampleDataVolume sd `shiftL` 4) .|. ((sdt `shiftR` 8) .&. 0x0f)
            putAsWord8 $ sdt .&. 0xff

sizeSampleData :: [SampleData] -> Int
sizeSampleData sds =  3 * length sds

showPT2Sample :: Sample -> [String]
showPT2Sample s = [ padSRight (width32 + 2) $ "Sample: " ++ show (sampleNumber s)
                  , replicate width32 '-' ]
                  ++ showSampleData (sampleData s)
                  ++ [ footer start l ]
        where
            start = sampleLoopStart s
            l = length (sampleData s)
            width = 2
            width32 = width * l
            footer 0 _ = replicate width32 '-'
            footer s e = if s < e-1 then out (s - 1) (e - s + 1) '-' '#' else replicate width32 '-'
            out n1 n2 c1 c2 = replicate (width * n1) c1 ++ replicate (width * n2) c2

showSampleData :: [SampleData] -> [String]
showSampleData ss = [ concat [showVolume s i | s <- ss ] | i <- [1..15] ]
    ++ [ [ showMask c $ m s | s <- ss, (c, m) <- [('T', sampleDataToneEnable), ('N', sampleDataNoiseEnable)] ]
       , foldr (showsHex 2) "" $ map sampleDataNoise ss
       , foldr (showsSgnHex 4 ) "" [sampleDataTone s | (n, s) <- zip [1..] ss, odd n]
       , "  " ++ foldr (showsSgnHex 4 ) "" [sampleDataTone s | (n, s) <- zip [1..] ss, even n]
       ]
    where
        showMask c m = if m then c else '.'
        showVolume s i = if sampleDataVolume s >= 16 - i then "[]" else ".."


---------------
getOrnaments :: Get [Ornament]
getOrnaments = getInstruments 16

putOrnaments :: [Ornament] -> (Put, [Int])
putOrnaments = putInstruments 16

instance InstrumentData OrnamentData where
    getInstrumentData = getOrnamentData
    putInstrumentData = putOrnamentData
    sizeInstrumentData = length
    showInstrumentData = showOrnamentData

getOrnamentData :: Get OrnamentData
getOrnamentData = do
    t <- getAsInt8
    return newOrnamentData{ ornamentDataTone = t }

putOrnamentData :: [OrnamentData] -> Put
putOrnamentData od = mapM_ (putAsInt8 . ornamentDataTone) od

showPT2Ornament :: Ornament -> [String]
showPT2Ornament orn = let o = concat $ showOrnamentData $ ornamentData orn
                        in [padSRight (length o) $ "Ornament: " ++ (show $ ornamentNumber orn), o]

showOrnamentData ds = [showsSgnInt 3 (ornamentDataTone d) " " |  d <- ds]
-------------------------------

