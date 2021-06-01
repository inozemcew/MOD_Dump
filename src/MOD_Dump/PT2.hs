module MOD_Dump.PT2 (pt2Module) where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

pt2Module :: Module
pt2Module = newModule
    { moduleExts = [".pt2"]
    , getData = getPT2ModuleData
    , showRow = showPT2Row
    , patternSep = "--+----+--------+--------+--------+ "
    , showSample = showPT2Sample
    , showOrnament = showPT2Ornament }

getPT2ModuleData :: Get ModuleData
getPT2ModuleData = do
    delay' <- fromIntegral <$> getWord8
    len <- fromIntegral <$> getWord8
    loopingPos' <- fromIntegral <$> getWord8
    x <- getWord16le
    guard (x == 0)
    samples' <- getSamples
    oorn0 <- getWord16le
    orn0 <- lookAhead $ do
        skipTo $ fromIntegral oorn0
        liftM2 (,) getWord16le getWord8
    guard (orn0 == (1,0))
    ornaments' <- getOrnaments
    pOffset <- fromIntegral <$> getWord16le
    title' <- B.unpack <$> getLazyByteString 30
    poss <- replicateM len $ fromIntegral <$> getWord8
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

        showsEForm = if noteEnvForm n /= EnvFormNone
                        then showsHex 1 (fromEnum $ noteEnvForm n)
                        else maybe ('0':) (const ('F':)) $  noteOrnament n


getPatterns :: Int -> Get [Pattern]
getPatterns n = forM [0..n] $ \i -> do
    here <- bytesRead
    chs <- replicateM 3 $ do
        ch <- fromIntegral <$> getWord16le
        guard (ch > fromIntegral here)
        return ch
    channels <- mapM (lookAhead . getChannel) chs
    return newPattern
        { patternNumber = i
        , patternRows = makeRowsWithShared makeShared channels }

makeShared :: [Note] -> Shared
makeShared notes = newShared { sharedEnvFreq = foldl (\a x -> if noteEnvFreq x == noEnvFreq then a else noteEnvFreq x) noEnvFreq notes }

getChannel :: Int -> Get Channel
getChannel offset = do
    skipTo offset
    evalStateT getNewNotes 0

getNewNotes :: StateT Int Get [Note]
getNewNotes = getNotes newNote

getNotes :: Note -> StateT Int Get [Note]
getNotes note = do
    n <- lift getWord8
    switch $ fromIntegral n
        where
            switch x
                | x == 0x00 = return []
                | x >= 0xe1 = getNotes $ note { noteSample = Just (x - 0xe0) }
                | x == 0xe0 = yieldNotes $ note { notePitch = Pause }
                | x >= 0x80 = yieldNotes $ note { notePitch = toEnum $ x - 0x80 + fromEnum pitchC1 }
                | x == 0x7f = getNotes $ note { noteEnvForm = EnvFormNone }
                | x >= 0x71 = do
                    freq <- lift getWord16le
                    getNotes $ note { noteEnvForm = toEnum (x - 0x70), noteEnvFreq = fromIntegral freq }
                | x == 0x70 = yieldNotes note
                | x >= 0x60 = getNotes $ note { noteOrnament = Just (x - 0x60) }
                | x >= 0x20 = put (x - 0x20) >> getNotes note
                | x >= 0x10 = getNotes $ note { noteVolume = Just (x - 0x10) }
                | x == 0x0f = do
                    d <- lift getWord8
                    getNotes $ note { noteCmd = NoteCmdDelay (fromIntegral d) }
                | x == 0x0e = do
                    g <- lift getInt8
                    getNotes note { noteCmd = NoteCmdGlis (fromIntegral g) }
                | x == 0x0d = do
                    g <- lift getWord8
                    f <- lift getInt16le
                    getNotes $ note { noteCmd = NoteCmdPorta (fromIntegral g) }
                | x == 0x0c = getNotes $ note { noteCmd = NoteCmdNone }
                | x == 0x0b = do
                    g <- lift getWord8
                    getNotes $ note { noteCmd = NoteCmdNoise (fromIntegral g) }
                | otherwise = getNotes note

            yieldNotes note = do
                r <- get
                ns <- getNewNotes
                return $ note : replicate r newNote ++ ns


------------------------------

getInstruments :: (InstrumentData d) => Int -> Get [Instrument d]
getInstruments n = forM [1..n-1] $ \i -> do
    offset <- fromIntegral <$> getWord16le
    if offset == 0
       then return $ newInstrument {instrumentNumber = i}
       else lookAhead $  do
            skipTo offset
            len <- fromIntegral <$> getWord8
            loopStart <- fromIntegral <$> getWord8
            d <- replicateM len getInstrumentData
            return $ newInstrument
                { instrumentNumber = i
                , instrumentData = d
                , instrumentLoopStart = loopStart }

class InstrumentData d where
    getInstrumentData :: Get d

getSamples :: Get [Sample]
getSamples = getInstruments 32

instance InstrumentData SampleData where
    getInstrumentData = getSampleData

getSampleData :: Get SampleData
getSampleData = do
    b0 <- fromIntegral <$> getWord8
    b1 <- fromIntegral <$> getWord8  -- bits 7-4 = volume, bits 3-0 tone high nibble
    b2 <- fromIntegral <$> getWord8  -- tone low byte
    let noiseMask = not $ b0 `testBit` 0
    let toneMask  = not $ b0 `testBit` 1
    let tone' = b2 + b1 `shiftL` 8 .&. 0xfff
    let tone = if b0 `testBit` 2 then tone' else negate tone'
    let noise = b0 `shiftR` 3
    let volume = b1 `shiftR` 4

    return newSampleData
                { sampleDataNoiseMask = noiseMask
                , sampleDataToneMask = toneMask
                , sampleDataTone = tone
                , sampleDataNoise = noise
                , sampleDataVolume = volume }

showPT2Sample :: Sample -> [String]
showPT2Sample s = [ padSRight width32 $ "Sample: " ++ show (sampleNumber s)
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
showSampleData ss = [concat [showVolume s i | s <- ss ] | i <- [1..15]]
                 ++ [concatMap (\s -> [showMask 'T' (sampleDataToneMask s), showMask 'N' (sampleDataNoiseMask s)]) ss]
                 ++ [foldr (\x ->  showsHex 2 x) "" $ map sampleDataNoise ss]
                    where
                         showMask c m = if m then c else '.'
                         showVolume s i = if sampleDataVolume s >= 16 - i then "[]" else ".."

---------------
getOrnaments :: Get [Ornament]
getOrnaments = getInstruments 16

instance InstrumentData OrnamentData where
    getInstrumentData = getOrnamentData

getOrnamentData :: Get OrnamentData
getOrnamentData = do
    t <- getInt8
    return newOrnamentData{ ornamentDataTone = fromIntegral t }

showPT2Ornament :: Ornament -> [String]
showPT2Ornament orn = let o = concat [showsSgnInt 3 (ornamentDataTone d) " " |  d <- ornamentData orn]
                        in [padSRight (length o) $ "Ornament: " ++ (show $ ornamentNumber orn), o]


-------------------------------

