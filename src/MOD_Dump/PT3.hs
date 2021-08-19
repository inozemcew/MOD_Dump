module MOD_Dump.PT3 where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Bits
import Data.List(mapAccumR)


pt3Module :: Module
pt3Module = newModule
    { moduleExts = [".pt3"]
    , getData = getPT3ModuleData
    , putData = putPT3ModuleData
    , showRow = showPT3Row
    , patternSep = "--+----+-------------+-------------+-------------+ "
    , showSample = showPT3Sample
    , showOrnament = showPT3Ornament }

getPT3ModuleData :: Get ModuleData
getPT3ModuleData = do
    tt <- B.unpack <$> getLazyByteString 63
    let (mtype', title') = splitTypeTitle tt
    replicateM 3 getWord8
    author' <- B.unpack <$> getLazyByteString 33
    toneTableType <- getAsWord8
    delay' <- getAsWord8
    songEnd' <- getAsWord8
    loopingPos' <- getAsWord8
    patternsOffset' <- getAsWord16le
    samples' <- getSamples
    ornaments' <- getOrnaments
    positions' <- whileM (<255) getAsWord8
    guard (all (\x -> x `mod` 3 == 0) positions')
    let positions'' = [ x `div` 3 | x <- positions' ]
    pos <- bytesRead
    guard (pos == patternsOffset')
    patterns' <- getPatterns $ maximum positions''
    return newModuleData
        { mtype = mtype'
        , title = trimR title'
        , author = trimR author'
        , delay = delay'
        , songEnd = songEnd'
        , loopingPos = loopingPos'
        , positions = [ newPosition { positionNumber = i} | i <- positions'' ]
        , patterns = patterns'
        , samples = filter (\s -> not.null $ sampleData s) samples'
        , ornaments = filter (\o -> ornamentNumber o > 0 && (not.null $ ornamentData o)) ornaments'
        , freqTType = Just $ toEnum toneTableType
        }

putPT3ModuleData :: ModuleData -> Put
putPT3ModuleData md = do
    putLazyByteString $ B.pack $ mtype md ++ " compilation of "
    putLazyByteString $ B.pack $ padSRight 32 $ title md
    putLazyByteString $ B.pack $ " by " ++ (padSRight 33 $ author md)
    putAsWord8 $ maybe 0 fromEnum $ freqTType md
    putAsWord8 $ delay md
    putAsWord8 $ songEnd md
    putAsWord8 $ loopingPos md
    putAsWord16le patOffset
    putWord16le 0  -- 0th sample placeholder - allways 0
    oo <- foldM foldOffs chStart samplesLengths
    putAsWord16le oo -- 0th ornament, allways the same empty ornament
    foldM_ foldOffs (oo+3) ornamentsLengths
    forM_ (positions md) $ putAsWord8 .(*3). positionNumber               -- put positions muliplied by 3
    putWord8 255                                                          -- end of positions table
    forM_ chsOffs putAsWord16le
    doPutPatterns
    doPutSamples
    mapM_ putWord8 [00,01,00]
    doPutOrnaments
        where
            (doPutSamples, samplesLengths) = putSamples $ samples md
            (doPutOrnaments, ornamentsLengths) = putOrnaments $ ornaments md
            patOffset = 202 + length (positions md)
            (doPutPatterns, channelsLengths) = putPatterns $ patterns md
            (chsOffs, chStart) = runState calcOffsets $ 2 * length channelsLengths +  patOffset

            foldOffs s o = do
                if o==0 then putWord16le 0 else putAsWord16le $ s
                return $ s + o

            calcOffsets = forM channelsLengths $ \l -> case l of
                                                            Left x  -> return $ chsOffs !! (x-1)
                                                            Right x -> state (\s -> (s, x + s))
splitTypeTitle:: String -> (String, String)
splitTypeTitle s = (foldr takeToLastDigit "" $ take 30 s, drop 30 s)
    where
        takeToLastDigit x a = if null a then if x `elem` ['0'..'9'] then x:a else a else x:a
        lastDigitPos = fst $ foldl (\(a,b) x -> if x `elem` ['0'..'9'] then (b+1,b+1)
                                                                       else (a,b+1)
                                   ) (0,0) $ take 30 s


---------------------------

getPatterns::Int -> Get [Pattern]
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
        chs = [ ch | pt <- pts, ch <- channelsFromRows $ patternRows pt ]
        nchs = zip [1..] chs
        findSame (n,ch) = fst $ head $ dropWhile (\(i,c) -> c /= ch && i < n) nchs
        foldChannel (n, ch) (ap, al) = let (p, l) = putChannel ch
                                           f = findSame (n, ch)
                                       in  if f == n then (p >> ap, Right l : al)
                                                     else (ap, Left f : al)

makeShared :: [Note] -> Shared
makeShared notes = let newE a x = if noteEnvFreq x == noEnvFreq then a else noteEnvFreq x
                       newN a x = if noteNoise x == noNoise then a else noteEnvFreq x
                       (ef,nf) = foldl (\(oe,on) n -> (newE oe n, newN on n)) (noEnvFreq,noNoise) notes
                   in
                       newShared { sharedEnvFreq = ef, sharedNoise = nf }


getChannel :: Int -> Get Channel
getChannel offset = do
    skipTo offset
    evalStateT getNewNotes (0,[])

putChannel :: [Note] -> (Put, Int)
putChannel ch = execState (foldM doPutNote newNote (packChannel ch) >> doModify1 0 ) (mempty, 0)
    where
        doPutNote oN (nN, mc) = do
            let oNS = noteSample oN
            let nNS = noteSample nN
            let oNO = noteOrnament oN
            let nNO = noteOrnament nN

            case (noteSample nN, noteOrnament nN, noteEnvForm nN) of
                 (Just s, Just o, e) -> do
                     doModify1 $ 0xf0 + o
                     doModify1 $ 2 * s
                     when (e /= Nothing) $ do
                         doModifyM 0xb0 $ fromEnum <$> e
                         doModify 2 $ putAsWord16be $ noteEnvFreq nN

                 (Nothing, Nothing, Just EnvFormNone) -> doModify1 $ 0xb0
                 (Just s, Nothing, Just EnvFormNone) -> do
                     doModify1 0x10
                     doModify1 $ s * 2
                 (Nothing, Nothing, Just e) -> do
                     doModify1 $ 0xb0 + fromEnum e
                     doModify 2 $ putAsWord16be $ noteEnvFreq nN
                 (Just s, Nothing, Just e) -> do
                     doModify1 $ 0x10 + fromEnum e
                     doModify 2 $ putAsWord16be $ noteEnvFreq nN
                     doModify1 $ s * 2
                 (Nothing, Just o, Nothing) -> doModify1 $ 0x40 + o
                 (Just s, Nothing, Nothing) -> doModify1 $ 0xd0 + s
                 _ -> return ()
            when (noteVolume oN /= noteVolume nN) $ doModifyM 0xc0 $ noteVolume nN
            when (noteCmd nN /= NoteCmdNone) $ case noteCmd nN of
                    NoteCmdGlisUp _ _ -> doModify1 0x01
                    NoteCmdGlisDn _ _ -> doModify1 0x01
                    NoteCmdPorta _ _ _ -> doModify1 0x02
                    _ -> doModify1 255
            maybe (return ()) (\x -> doModify2 0xb1 (x+1)) mc
            doModify1 $ putPitch $ notePitch nN
            when (noteCmd nN /= NoteCmdNone) $ case noteCmd nN of
                    NoteCmdGlisUp n v -> do
                        doModify1 n
                        doModify 2 $ putAsInt16le v
                    NoteCmdGlisDn n v -> do
                        doModify1 n
                        doModify 2 $ putAsInt16le v
                    NoteCmdPorta v1 v2 x ->  do
                        doModify1 x
                        doModify 2 $ putAsWord16le v1
                        doModify 2 $ putAsInt16le v2
                    _ -> doModify1 255
            return nN

        putPitch Release = 0xe0
        putPitch Pause = 0xc0
        putPitch NoNote = 0xd0
        putPitch n = fromEnum n - fromEnum pitchC1 + 0x50

        doModify x m = modify $ \(p, l) -> (p >> m, l + x)
        doModify1 x = doModify 1 $ putAsWord8 x
        doModify2 c n = doModify 2 $ putWord8 c >> putAsWord8 n
        doModifyM x mz = maybe (lift mempty) (\y -> doModify1 (x + y)) mz

getNewNotes :: StateT (Int,[Int]) Get [Note]
getNewNotes = getNotes newNote

getNotes :: Note -> StateT (Int,[Int]) Get [Note]
getNotes note = do
    n <- lift getAsWord8
    switch n
        where
            switch x
                | x == 0x00 = return []
                | x >= 0xf0 = do
                                s <- lift getAsWord8
                                getNotes $ note { noteOrnament = Just (x - 0xf0)
                                                , noteSample = Just (s `div` 2)
                                                , noteEnvForm = Nothing }
                | x >= 0xd1 = getNotes $ note { noteSample = Just (x - 0xd0) }
                | x == 0xd0 = yieldNotes note
                | x >= 0xc1 = getNotes $ note { noteVolume = Just (x - 0xc0) }
                | x == 0xc0 = yieldNotes $ note { notePitch = Pause }
                | x >= 0xb2 = do
                                freq <- lift getAsWord16be -- reverse byte order here
                                getNotes $ note { noteEnvForm = Just $ toEnum (x - 0xb0)
                                                , noteEnvFreq = freq
                                                }
                | x == 0xb1 = do
                                s <- lift getAsWord8
                                modify $ \(_, fx) -> (s - 1, fx)
                                getNotes note
                | x == 0xb0 = getNotes $ note { noteEnvForm = noEnvForm }
                | x >= 0x50 = yieldNotes $ note { notePitch = toEnum $ x - 0x50 + fromEnum pitchC1 }
                | x >= 0x40 = getNotes $ note { noteOrnament = Just (x - 0x40) }
                | x >= 0x20 = getNotes $ note { noteNoise = x - 0x20 }
                | x >= 0x11 = do
                                e <- lift getAsWord16be
                                s <- lift getAsWord8
                                getNotes note
                                    { noteEnvForm = Just $ toEnum (x - 0x10)
                                    , noteEnvFreq = e
                                    , noteSample = Just (s `div` 2)
                                    }
                | x == 0x10 = do
                                s <- lift getAsWord8
                                getNotes note
                                    { noteEnvForm = Just EnvFormNone
                                    , noteSample = Just (s `div` 2)
                                    }
                | x <= 0x09 = do
                                modify $ \(s,fx) -> (s,x:fx)
                                getNotes note
                | otherwise = getNotes note

            yieldNotes :: Note -> StateT (Int,[Int]) Get [Note]
            yieldNotes note = do
                (r,fx) <- get
                n <- lift $ execStateT (forM fx getFxParams) note
                put (r,[])
                ns <- getNewNotes
                return $ n : replicate r newNote ++ ns

            getFxParams fx = do
                case fx of
                    1 -> do
                            x <- lift getAsWord8
                            v <- lift getAsInt16le
                            modify $ \n -> n { noteCmd = (if v<0 then NoteCmdGlisUp else NoteCmdGlisDn) x v }
                    2 -> do
                            x <- lift getAsWord8
                            v1 <- lift getAsWord16le  -- Target tone, not used
                            v2 <- lift getAsInt16le
                            modify $ \n -> n { noteCmd = NoteCmdPorta v1 v2 x }
                    3 -> do
                            x <- lift getAsWord8
                            modify $ \n -> n { noteCmd = NoteCmdSampleOffset x }
                    4 -> do
                            x <- lift getAsWord8
                            modify $ \n -> n { noteCmd = NoteCmdOrnamentOffset x }
                    5 -> do
                            x <- lift getAsWord8
                            y <- lift getAsWord8
                            modify $ \n -> n { noteCmd = NoteCmdVibrato x y }
                    8 -> do
                            x <- lift getAsWord8
                            e <- lift getAsWord16le
                            modify $ \n -> n { noteCmd = NoteCmdEnvSlide x e }
                    9 -> do
                            x <- lift getAsWord8
                            modify $ \n -> n { noteCmd = NoteCmdDelay x }
                    _ -> return ()

showPT3Row :: Row -> String
showPT3Row r = shows2 (rowNumber r) . ('|':) . showsShared (rowShared r)
               $ (foldr (\x -> ('|':) . showsNote x) "|" (rowNotes r))

showsShared :: Shared -> ShowS
showsShared s = showsHex 4 (sharedEnvFreq s)

showsNote :: Note -> ShowS
showsNote n = showsPitch (notePitch n) .(' ':)
            . showsHex 1 (maybe 0 id $ noteSample n)
            . showsEForm
            . showsHex 1 (maybe 0 id $ noteOrnament n)
            . showsHex 1 (maybe 0 id $ noteVolume n)
            .(' ':) . showsCmd (noteCmd n)
    where
        showsPitch (Pitch k o ) = showString (["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"] !! fromEnum k) . shows o
        showsPitch Pause = showString "R--"
        showsPitch _ = showString "---"

        showsCmd (NoteCmdGlisUp x v) = ('1':) .showsHex 1 x .showsHex 2 v
        showsCmd (NoteCmdGlisDn x v) = ('2':) .showsHex 1 x .showsHex 2 v
        showsCmd (NoteCmdPorta _ v2 x) = ('3':) .showsHex 1 x .showsHex 2 v2
        showsCmd (NoteCmdSampleOffset x) = showString "40" . showsHex 2 x
        showsCmd (NoteCmdOrnamentOffset x) = showString "50" . showsHex 2 x
        showsCmd (NoteCmdVibrato x y) = showString "60" . showsHex 1 x . showsHex 1 y
        showsCmd (NoteCmdEnvSlide d x) = ((if x>0 then '9' else 'A'):) . showsHex 1 d . showsHex 3 x
        showsCmd (NoteCmdDelay x) = showString "B0" . showsHex 2 x
        showsCmd _ = showString "0000"

        showsEForm = if noteEnvForm n `elem` [Nothing, noEnvForm]
                        then maybe ('0':) (const ('F':)) $  noteOrnament n
                        else let (Just ef) =  noteEnvForm n in showsHex 1 $ fromEnum ef


---------------------------------

getInstruments :: (InstrumentData d) => Int -> Get [Instrument d]
getInstruments n = forM [0..n-1] $ \i -> do
    offset <- getAsWord16le
    if offset == 0
       then return $ newInstrument {instrumentNumber = i}
       else lookAhead $  do
            skipTo offset
            loop <- getAsWord8
            len <- getAsWord8
            d <- replicateM len getInstrumentData
            return $ newInstrument
                { instrumentNumber = i
                , instrumentData = d
                , instrumentLoopStart = loop }

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
putInstrument i = do
            putAsWord8 $ instrumentLoopStart i
            let ids = instrumentData i
            putAsWord8 $ length ids
            mapM_ putInstrumentData ids

class InstrumentData d where
    getInstrumentData :: Get d
    putInstrumentData :: d -> Put
    sizeInstrumentData :: [d] -> Int

instance InstrumentData SampleData where
    getInstrumentData = getSampleData
    putInstrumentData = putSampleData
    sizeInstrumentData = sizeSampleData


getSamples :: Get [Sample]
getSamples = getInstruments 32

putSamples :: [Sample] -> (Put, [Int])
putSamples = putInstruments 32


getSampleData :: Get SampleData
getSampleData = do
    b1 <- getWord8
    b2 <- getWord8
    tFreq <- getAsInt16le
    return $ newSampleData
        { sampleDataNoise       = fromIntegral $ 31 .&. shiftR b1 1
        , sampleDataTone        = tFreq
        , sampleDataVolume      = fromIntegral $ 15 .&. b2
        , sampleDataNoiseEnable = not $ testBit b2 7
        , sampleDataToneHold    = testBit b2 6
        , sampleDataNoiseHold   = testBit b2 5
        , sampleDataToneEnable  = not $ testBit b2 4
        , sampleDataEnvEnable   = not $ testBit b1 0
        , sampleDataEffect = case (testBit b1 7, testBit b1 6) of
                                    (True, False) -> SDEDown
                                    (True, True) -> SDEUp
                                    _ -> SDENone
        }

putSampleData :: SampleData -> Put
putSampleData sd = do
    putWord8
        $ changeBit 7 (sampleDataEffect sd == SDEUp || sampleDataEffect sd == SDEDown)
        $ changeBit 6 (sampleDataEffect sd == SDEUp)
        $ changeBit 0 (not $ sampleDataEnvEnable sd)
        $ fromIntegral $ sampleDataNoise sd `shiftL` 1
    putWord8
        $ changeBit 7 (not $ sampleDataNoiseEnable sd)
        $ changeBit 6 (sampleDataToneHold sd)
        $ changeBit 5 (sampleDataNoiseHold sd)
        $ changeBit 4 (not $ sampleDataToneEnable sd)
        $ fromIntegral $ sampleDataVolume sd
    putAsInt16le $ sampleDataTone sd

showPT3Sample::Sample -> [String]
showPT3Sample s = let width53 = 38 in
                      [ padSRight width53 $ "Sample " ++ showsHex 2 (sampleNumber s) "", replicate width53 '-' ]
                      ++ showSampleData (sampleLoopStart s) (sampleData s)
                      ++ [ replicate width53 '-' ]

sizeSampleData :: [SampleData] -> Int
sizeSampleData sds = 4 * length sds


showSampleData:: Int -> [SampleData] -> [String]
showSampleData l sds = [ shows2 i
                      .showsLoop l i
                      .showsFlags s .(' ':)
                      .showsSgnInt 5 (sampleDataTone s) .(' ':)
                      .shows2 (sampleDataNoise s) .(' ':)
                      .showsMasks s .(' ':)
                      .showsVol (sampleDataVolume s) $ ""
                      | (i,s) <- zip [0..] sds]
    where
        showsFlags s = let  f1 = case sampleDataEffect s of
                                SDEDown -> '-'
                                SDEUp -> '+'
                                _ -> '0'
                            f2 = '0'
                            f3 = '0'
                       in \x -> f1:f2:f3:x
        showsMasks s = \x -> showMask 'T' (sampleDataToneEnable s)
            : showMask 'N' (sampleDataNoiseEnable s)
            : showMask 'E' (sampleDataEnvEnable s )
            : x
        showMask c m = if m then c else '-'
        showsVol v = showString (concat $ replicate v "#") . showString (concat $ replicate (16 - v) ".") . (' ':) .showsHex 1 v

-------------------------------
getOrnaments :: Get [Ornament]
getOrnaments = getInstruments 16

putOrnaments :: [Ornament] -> (Put, [Int])
putOrnaments = putInstruments 16

instance InstrumentData OrnamentData where
    getInstrumentData = getOrnamentData
    putInstrumentData = putOrnamentData
    sizeInstrumentData = sizeOrnamentData

getOrnamentData :: Get OrnamentData
getOrnamentData = do
    t <- getAsInt8
    return newOrnamentData{ ornamentDataTone = t }

putOrnamentData :: OrnamentData -> Put
putOrnamentData od = putAsWord8 $ ornamentDataTone od

sizeOrnamentData :: [OrnamentData] -> Int
sizeOrnamentData ods = length ods

showPT3Ornament :: Ornament -> [String]
showPT3Ornament orn = let o = [ shows2 i
                              . showsLoop (ornamentLoopStart orn) i
                              . showsSgnInt 3 (ornamentDataTone d) $ " "
                                  |  (i, d) <- zip [0..] (ornamentData orn)]
                      in [padSRight (length o) $ "Ornament: " ++ (show $ ornamentNumber orn)] ++ o


-------------------------------

showsLoop :: Int -> Int -> ShowS
showsLoop l i = if i >= l then ('*':) else (' ':)
