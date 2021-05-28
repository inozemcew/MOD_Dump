module MOD_Dump.PT3 (readPT3Module) where

import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State


readPT3Module :: FilePath -> B.ByteString -> Maybe ShowModule
readPT3Module = readModule pt3Module

pt3Module :: Module
pt3Module = newModule 
    { moduleExts = [".pt3"]
    , getData = getPT3ModuleData
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
    toneTableType <- fromIntegral <$> getWord8
    delay' <- fromIntegral <$> getWord8
    songEnd' <- fromIntegral <$> getWord8
    loopingPos' <- fromIntegral <$> getWord8
    patternsOffset' <- fromIntegral <$> getWord16le
    samplesOffsets' <- replicateM 32 $ fromIntegral <$> getWord16le
    ornamentsOffsets' <- replicateM 16 $ fromIntegral <$> getWord16le
    positions' <- whileM (<255) getWord8
    guard (all (\x -> x `mod` 3 == 0) positions')
    let positions'' = [ fromIntegral x `div` 3 | x <- positions' ]
    pos <- bytesRead
    guard (pos == patternsOffset')
    patterns' <- getPatterns $ maximum positions''
    return newModuleData 
        { mtype = mtype'
        , title = trim title'
        , author = trim author'
        , delay = delay'
        , loopingPos = loopingPos'
        , positions = [ newPosition { positionNumber = i} | i <- positions'' ] 
        , patterns = patterns' }
        
splitTypeTitle:: String -> (String, String)
splitTypeTitle s = (take lastDigitPos s, drop 30 s) 
    where
        lastDigitPos = fst $ foldl (\(a,b) x -> if x `elem` ['0'..'9'] then (b+1,b+1) 
                                                                       else (a,b+1)
                                   ) (0,0) $ take 30 s


---------------------------

getPatterns::Int -> Get [Pattern]
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
    evalStateT getNewNotes (0,[])

getNewNotes :: StateT (Int,[Int]) Get [Note]
getNewNotes = getNotes newNote

getNotes :: Note -> StateT (Int,[Int]) Get [Note]
getNotes note = do
    n <- lift getWord8
    switch $ fromIntegral n
        where
            switch x
                | x == 0x00 = return []
                | x >= 0xf0 = do
                                s <- lift getWord8
                                getNotes $ note { noteOrnament = Just (x - 0xf0), noteSample = Just $ fromIntegral s `div` 2 }
                | x >= 0xd1 = getNotes $ note { noteSample = Just (x - 0xd0) }
                | x == 0xd0 = yieldNotes note
                | x >= 0xc0 = getNotes $ note { noteVolume = Just (x - 0xc0) }
                | x == 0xc0 = yieldNotes $ note { notePitch = Pause }
                | x >= 0xb2 = do
                                freq <- lift getWord16be -- reverse byte order here
                                getNotes $ note { noteEnvForm = toEnum (x - 0xb0), noteEnvFreq = fromIntegral freq }
                | x == 0xb1 = do
                                s <- lift getWord8
                                modify $ \(_, fx) -> (fromIntegral s - 1, fx)
                                getNotes note
                | x == 0xb0 = getNotes $ note { noteEnvForm = EnvFormNone }
                | x >= 0x50 = yieldNotes $ note { notePitch = toEnum $ x - 0x50 + fromEnum pitchC1 }
                | x >= 0x40 = getNotes $ note { noteOrnament = Just (x - 0x40) }
                | x >= 0x20 = getNotes $ note { noteCmd = NoteCmdNoise $ x - 0x20 }
                | x >= 0x10 = do
                                e <- lift getWord16be
                                s <- lift getWord8
                                getNotes note 
                                    { noteEnvForm = toEnum (x - 0x10)
                                    , noteEnvFreq = fromIntegral e
                                    , noteSample = Just $ fromIntegral (s `div` 2) 
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
                            x <- fromIntegral <$> lift getWord8
                            v <- lift getInt16le
                            modify $ \n -> n { noteCmd = (if v<0 then NoteCmdGlisUp else NoteCmdGlisDn) (x * 0x100 + fromIntegral v) }
                    2 -> do
                            x <- fromIntegral <$> lift getWord8
                            lift getWord16le  -- Target tone, not used
                            v <- lift getWord16le
                            modify $ \n -> n { noteCmd = NoteCmdPorta (x * 0x100 + fromIntegral v) }
                    3 -> do 
                            x <- fromIntegral <$> lift getWord8
                            modify $ \n -> n { noteCmd = NoteCmdSampleOffset x }
                    4 -> do 
                            x <- fromIntegral <$> lift getWord8
                            modify $ \n -> n { noteCmd = NoteCmdOrnamentOffset x }
                    5 -> do
                            x <- fromIntegral <$> lift getWord8
                            y <- lift getWord8
                            modify $ \n -> n { noteCmd = NoteCmdVibrato x (fromIntegral y) }
                    8 -> do
                            x <- fromIntegral <$> lift getWord8
                            e <- lift getWord16le
                            modify $ \n -> n { noteCmd = NoteCmdEnvSlide x (fromIntegral e) }
                    9 -> do 
                            x <- fromIntegral <$> lift getWord8
                            modify $ \n -> n { noteCmd = NoteCmdDelay x }
                    _ -> return ()

showPT3Row::Row -> String
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

        showsCmd (NoteCmdGlisUp x) = ('1':) .showsHex 3 x
        showsCmd (NoteCmdGlisDn x) = ('2':) .showsHex 3 x
        showsCmd (NoteCmdPorta x) = ('3':) .showsHex 3 x
        showsCmd (NoteCmdSampleOffset x) = showString "40" . showsHex 2 x
        showsCmd (NoteCmdOrnamentOffset x) = showString "50" . showsHex 2 x
        showsCmd (NoteCmdVibrato x y) = showString "60" . showsHex 1 x . showsHex 1 y
        showsCmd (NoteCmdEnvSlide d x) = ((if x>0 then '9' else 'A'):) . showsHex 1 d . showsHex 3 x
        showsCmd (NoteCmdDelay x) = showString "B0" . showsHex 2 x 
        showsCmd _ = showString "0000"

        showsEForm = if noteEnvForm n /= EnvFormNone
                        then showsHex 1 (fromEnum $ noteEnvForm n)
                        else maybe ('0':) (const ('F':)) $  noteOrnament n


showPT3Sample::Sample -> [String]
showPT3Sample _ = [] 

showPT3Ornament::Ornament -> [String]
showPT3Ornament _ = [] 
