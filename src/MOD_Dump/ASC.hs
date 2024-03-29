module MOD_Dump.ASC where

-- ASC module format:
    -- +0 = Delay :: Byte
    -- +1 = Looping position :: Byte
    -- +2 = Patterns table offset :: Word
    -- +4 = Sample table offset :: Word
    -- +6 = Image table offset :: Word
    -- +8 = Number of positions :: Byte
    -- +9 = Positions :: [Byte]::`Number of positions`
    --
    -- if `Patterns table offset` - `Number of positions` == 72 then
    --      +(`Patterns table offset`) - 44 = Title :: [Char]::20
    --      +(`Patterns table offset`) - 20 = Author :: [Char]::20
    --
    -- +(`Patterns table offset`) = Pattern channels offsets :: [(Word, Word, Word)]
    -- +(`Patterns table offset`) + (`Pattern channel offset`) = Pattern data
    --
    -- +(`Sample table offset`) = Samples' offsets :: [Word]::32
    -- +(`Sample table offset`) + (`Sample offset`) = Sample data
    --
    -- +(`Image table offset`) = Images' offsets :: [Word]::32
    -- +(`Image table offset`) + (`Image offset`) = Image data

    -- Pattern data :: [ByteString] =
        --                  | 0x00 .. 0x55 = Note, if volume == 'EN' then followed by byte of envelope period
        --                  | 0x56 .. 0x5d = Empty note
        --                  | 0x5d         = Release sample
        --                  | 0x5e         = Pause
        --                  | 0x60 .. 0x9f = Skip x rows after note or relese or pause
        --                  | 0xa0 .. 0xbf = Set current sample
        --                  | 0xc0 .. 0xdf = Set current image
        --                  | 0xe0         = Set current volume as 'EN'
        --                  | 0xe1 .. 0xef = Set current volume 1..f
        --                  | 0xf0, n      = Set noise period as `n`
        --                  | 0xf1         = Hold sample command
        --                  | 0xf2         = Hold image command
        --                  | 0xf3         = Hold sample and image
        --                  | 0xf4, n      = Set delay as `n` command
        --                  | 0xf5, n      = GlisUp `n` command
        --                  | 0xf6, n      = GlisDn `n` command
        --                  | 0xf7, n      = Port.S+ `n` command
        --                  | 0xf8         = Set envelope form 8 '\'
        --                  | 0xf9, n      = Port.S- command
        --                  | 0xfa         = Set envelope form 10 'V'
        --                  | 0xfb, n      = Volume slide 1..1f bit5 = sign
        --                  | 0xfc         = Set envelope form 12 '/'
        --                  | 0xfe         = Set envelope form 14 '^'

    -- Sample data :: [(Byte, Byte, Byte)]
        -- +0 = | bit 7 == 1 = Loop start point
        --      | bit 6 == 1 = Loop end point
        --      | bit 5 == 1 = sample end
        --      | bit 4..0 = noise deviation
        -- +1 = pitch deviation
        -- +2 = | bit 0 == 1 = tone present
            --  | bit 2,1 ==    | 00 = No volume change
                            --  | 01 = Envelope enabled
                            --  | 10 = Decrease volume
                            --  | 11 = Increase volume
            --  | bit 3 == 1 = noise present
            --  | bit 4..7 = current volume

    -- Image data :: [(Byte,Byte)]
        -- +0 = | bit 7 == 1 = Loop start point
        --      | bit 6 == 1 = Loop end point
        --      | bit 5 == 1 = image end
        --      | bit 4..0 = noise deviation
        -- +1 = note deviation


import MOD_Dump.Elements
import MOD_Dump.Module
import MOD_Dump.Utils
import MOD_Dump.FlagSet
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import GHC.Word
import Data.List(intercalate, transpose)
import Debug.Trace

ascModule :: Module
ascModule = newModule
    { moduleExts = [".asc",".C"]
    , getData = getASCModuleData
    , putData = putASCModule
    , showRow = showASCRow
    , patternSep = "---+--------+------------+------------+----------- "
    , showSample = showASCSample
    , showOrnament = showASCImage
    }

---------------
getASCModuleData :: Get ModuleData
getASCModuleData = do
            -- for module with built-in player get Title and Author from that player and skip to module data
            ta <- lookAheadM getMaybeTandAFromPlayer

            (d, l, tables, ps, t, a) <- lookAhead $ getHeader ta
            -- sample table can start only 9 or 72 bytes after positions table
            guard $ (patternsTable tables - length ps ==) `any` [9,72]

            s1 <- peekWord16 $ samplesTable tables
            guard (s1 == 0x40)

            i1 <- peekWord16 $ ornamentsTable tables
            guard (i1 == 0x40)

            let count = 1 + (positionNumber $ maximum ps)
            p1 <- peekWord16 $ patternsTable tables
            guard (count * 6 == p1)

            patterns' <- lookAhead $ getPatterns (patternsTable tables) count

            samples' <- lookAhead $ getSamples (samplesTable tables)

            ornaments' <- getImages (ornamentsTable tables)

            return newModuleData
                    { mtype = "ASC Sound master compiled song"
                    , delay =  d
                    , loopingPos = l
                    , positions = ps
                    , patterns = patterns'
                    , samples = samples'
                    , ornaments = ornaments'
                    , title = t
                    , author = a }

putASCModule :: ModuleData -> Put
putASCModule md = do
    let patternsTableOffset = length (positions md) + if all null [title md, author md] then 9 else 72
    let (doPutPatterns, patternsLength) = putPatterns md
    let (doPutSamples,  samplesLength)  = putSamples $ samples md
    let (doPutImages,_)                 = putImages $ ornaments md
    let t = newTables
            { patternsTable  = patternsTableOffset
            , samplesTable   = patternsTableOffset + patternsLength
            , ornamentsTable = patternsTableOffset + patternsLength + samplesLength
            }
    putHeader t md
    doPutPatterns
    doPutSamples
    doPutImages

getHeader :: Maybe (String, String) -> Get (Int, Int, Tables, [Position], String, String)
getHeader ta =  do
        d <- getAsWord8
        l <- getAsWord8

        p <- getAsWord16le
        s <- getAsWord16le
        o <- getAsWord16le
        let tables = newTables{ patternsTable = p, samplesTable = s, ornamentsTable = o }

        n <- getAsWord8
        ps <- replicateM n $  do
            x <- getAsWord8
            return $ newPosition { positionNumber = x }

        (t,a) <- maybe (getTAndA p ps) return ta
        return (d, l, tables, ps, t, a)

getTAndA :: Int -> [Position] -> Get (String, String)
getTAndA pt p = if pt - length p /= 72
                    then return ("","")
                    else do
                        skip 19
                        doGetTAndA

doGetTAndA :: Get (String, String)
doGetTAndA = do
    t <- getLazyByteString 20
    skip 4
    a <- getLazyByteString 20
    return (B.unpack t, B.unpack a)


getMaybeTandAFromPlayer :: Get (Maybe (String, String))
getMaybeTandAFromPlayer = runMaybeT $ do
        chkByte 11 (\x -> x == 0xc3 || x == 0x18)
        chkByte 2 (== 0xc3)
        chkByte 2 (== 0xc3)
        chkBString 2 $ B.pack "ASM COMPILATION OF "
        ta <- lift $ doGetTAndA
        lift $ skipWhile [0x28,0, 0x26,0, 0x24,0, 0x22,0, 0x20,0, 0x1E,0, 0x1C,0]
        return ta
    where
        chkByte n g = do
                x <- lift $ skip n >> getWord8
                guard $ g x

        chkBString n s = do
                s' <- lift $ skip n >> getLazyByteString (B.length s)
                guard (s' == s)

        skipWhile s = skipWhile' s s
            where
                skipWhile' _ [] = return ()
                skipWhile' s (x:xs) = do
                    w <- getWord8
                    if w == x then skipWhile' s xs else skipWhile' s s

putHeader :: Tables -> ModuleData -> Put
putHeader t md = do
    forM_ [delay, loopingPos] $ \f -> putAsWord8 $ f md
    putAsWord16le $ patternsTable t
    putAsWord16le $ samplesTable t
    putAsWord16le $ ornamentsTable t
    putAsWord8 $ length $ positions md
    forM_ (positions md) $ putAsWord8 . positionNumber
    when (any (not.null) [title md, author md]) $ do
        putLazyByteString $ B.pack "ASM COMPILATION OF "
        putLazyByteString $ B.pack $ padSRight 20 $ title md
        putLazyByteString $ B.pack $ " BY "
        putLazyByteString $ B.pack $ padSRight 20 $ author md

showASCHeader :: ModuleData -> [String]
showASCHeader m = [ "Song type: " ++ show (mtype m)
                  , "Song title: " ++ show (title m)
                  , "Composed: " ++ show (author m)
                  , "Delay: " ++ show (delay m)
                  , "Looping position: " ++ show (loopingPos m)
                  , "Positions: " ++ concatMap showPosition (positions m)
                  ]

-----------------------------------------------

showPosition pn = '{' : shows (positionNumber pn) "}"

putOffsets :: Int -> [Int] -> Put
putOffsets s xs = foldM_ (\a x -> putAsWord16le a >> return (a + x)) s xs

------------------
getPatterns:: Int -> Int -> Get [Pattern]
getPatterns offset count = do
    skip offset
    forM [0..count-1] $ \i -> do
        let i6 = i * 6 + 6
        offs <- replicateM 3 $ (\x -> x - i6) <$> getAsWord16le
        chs <- mapM (lookAhead.getChannel) offs
        return $ newPattern { patternNumber = i
                            , patternRows = makeRowsWithShared makeShared chs }


putPatterns :: ModuleData -> (Put, Int)
putPatterns md = (putOffs >> sequence_ pput, paternsOffsetsTableLength +  sum patternsLengths)
    where
        paternsOffsetsTableLength = 6 * length (patterns md)
        (pput, patternsLengths) = unzip [ putChannel ch | pt <- patterns md, ch <- channelsFromRows $ patternRows pt]
        putOffs = putOffsets paternsOffsetsTableLength patternsLengths


showASCRow :: Row -> String
showASCRow r = header . (\x -> foldr showsNote x $ rowNotes r) $ ""
    where
        s = rowShared r
        header = shows2 (rowNumber r) . (" | " ++)
            . showsEnvFreq (sharedEnvFreq s)
            . showsEnvForm (sharedEnvForm s)
            . showsNoiseMask (sharedMask s)
            . showsNoise (sharedNoise s) .(' ':)

-------------------------------------------------------------
showsPitch :: Pitch -> ShowS
showsPitch (Pitch n o) = let noteNames = ["_C","#C","_D","#D","_E","_F","#F","_G","#G","_A","#A","_B"]
                             octaveNames = "UCLS1234"
                         in showChar (octaveNames !! o) . showString ( noteNames !! fromEnum n)
showsPitch Pause = showString "PSE"
showsPitch Release = showString "RLS"
showsPitch _ = showString "___"

showsNoteCmd :: NoteCmd -> ShowS
showsNoteCmd NoteCmdNone = showChar '|'
showsNoteCmd _           = showChar '#'

showsEnvFreq :: EnvFreq -> ShowS
showsEnvFreq x = if x == 0 then ("___" ++) else shows3 x

showsEnvForm :: EnvForm -> ShowS
showsEnvForm EnvFormNone = showChar '_'
showsEnvForm x = lookupChar '_' [ (EnvFormRepDecay, '\\')
                                , (EnvFormRepDecayAttack, 'V')
                                , (EnvFormRepAttack, '/')
                                , (EnvFormRepAttackDecay, '^')
                                ] x

showsNoiseMask :: ChannelMask -> ShowS
showsNoiseMask x = if x == noChannelMask then ('_':) else showsB 1 $ channelMaskValue x

showsNoise :: Noise -> ShowS
showsNoise x = if x == noNoise then ('_':) else shows32 $ fromNoise x

---------------
makeShared :: [Note] -> Shared
makeShared abc = newShared {
    sharedEnvFreq = eFreq,
    sharedEnvForm = eForm,
    sharedMask = mask,
    sharedNoise = noise
}
    where
        findF f a x = if noteVolume x == 0 then f x else a
        eFreq = foldl (findF noteEnvFreq) noEnvFreq abc
        eForm = foldl (findF noteEnvForm) noEnvForm abc
        maskRShift1 m = m { channelMaskValue = channelMaskValue m `div` 2 + 4 }
        maskRShift0 m = m { channelMaskValue = channelMaskValue m `div` 2 }
        maskShift (n,m) x = let nn = noteNoise x in if nn == noNoise then (n, maskRShift1 m) else (nn, maskRShift0 m)
        (noise,mask) = foldl maskShift (noNoise, noChannelMask) abc

---------------
showsNote :: Note -> ShowS
showsNote note = showsNoteCmd (noteCmd note) .(' ':). showsPitch pitch .
    if isPitch pitch then (' ':). shows32 (noteSample note)
                         .(' ':). shows32 (noteOrnament note)
                         .(' ':). showsVolume (noteVolume note)
                         .(' ':)
                     else (" _ _ __ "++)
        where
            pitch = notePitch note
            showsVolume 0 = showString "EN"
            showsVolume v = shows2 v

---------------
getChannel :: Int -> Get Channel   --- Empty channel returns empty list - error!!
getChannel offset = skip offset >> evalStateT getNewNotes 0
    where
        getNewNotes = getNotes newNote
        getNotes n = do
            v <- lift getAsWord8
            switch n v

        switch n x
            | x == 255 = return []
            -- End of pattern

            | x <= 0x55 = yieldNote $ n{ notePitch = toEnum $ x + fromEnum pitchAS0 }
            -- Note, may be followed by env period if volume='EN'

            | x >= 0x56 && x <= 0x5d = yieldNote' $ n{ notePitch = NoNote }
            -- No note, just placeholder for empty channel

            | x == 0x5e = yieldNote' $ n{ notePitch = Release }
            -- Release sample

            | x == 0x5f = yieldNote' $ n{ notePitch = Pause }
            -- Pause

            | x >= 0x60 && x <= 0x9f = do
                put (x - 0x60)
                getNotes n
            -- Skip x rows after note or relese or pause

            | x >= 0xa0 && x <= 0xbf = getNotes $ n{ noteSample = (x - 0xa0)
                                                   , noteFlags = noteFlags n `set` ChangedSample }
            -- Set current sample
            | x >= 0xc0 && x <= 0xdf = getNotes $ n{ noteOrnament = (x - 0xc0)
                                                   , noteFlags = noteFlags n `set` ChangedOrnament }
            -- Set current image
            | x == 0xe0 = getNotes $ n { noteEnvEnable = True
                                       , noteVolume = 0
                                       , noteFlags = noteFlags n `set` ChangedVolume `set` ChangedEnvEnable }
            -- Set 0 volume and enable envelope

            | x >= 0xe1 && x <= 0xef = getNotes
                $ n{ noteVolume = (x - 0xe0)
                   , noteEnvEnable = False
                   , noteFlags = noteFlags n `set` ChangedVolume `set` ChangedEnvEnable }
            -- Set current volume

            | x == 0xf0 = do
                noise <- lift getWord8
                getNotes $ n{ noteNoise = toNoise noise
                            , noteFlags = noteFlags n `set` ChangedNoise }        -- Set noise period
            | x == 0xf1 = getNotes $ n{ noteCmd = NoteCmdHldSample }              -- Hold sample command
            | x == 0xf2 = getNotes $ n{ noteCmd = NoteCmdHldImage }               -- Hold image command
            | x == 0xf3 = getNotes $ n{ noteCmd = NoteCmdHldInstr }               -- Hold sample and image
            | x == 0xf4 = setCmd NoteCmdDelay                                     -- Set delay command
            | x == 0xf5 = setCmd $ NoteCmdGlisUp 0                                -- GlisUp command
            | x == 0xf6 = setCmd $ NoteCmdGlisDn 0                                -- GlisDn command
            | x == 0xf7 = setCmd NoteCmdPortaR                                    -- Port.S+ command
            | x == 0xf8 = getNotes
                $ n{ noteEnvForm = EnvFormRepDecay
                   , noteFlags = noteFlags n `set` ChangedEnvForm }               -- Set envelope form 8 '\'
            | x == 0xf9 = setCmd $ NoteCmdPorta 0 0                               -- Port.S- command
            | x == 0xfa = getNotes
                $ n{ noteEnvForm = EnvFormRepDecayAttack
                   , noteFlags = noteFlags n `set` ChangedEnvForm }               -- Set envelope form 10 'V'
            | x == 0xfb = do                                                      -- Volume slide
                d <- lift getWord8
                getNotes $ n{ noteCmd = NoteCmdVolSlide $ intExpand 32 d}
            | x == 0xfc = getNotes
                $ n{ noteEnvForm = EnvFormRepAttack
                   , noteFlags = noteFlags n `set` ChangedEnvForm }              -- Set envelope form 12 '/'
            | x == 0xfe = getNotes
                $ n{ noteEnvForm = EnvFormRepAttackDecay
                   , noteFlags = noteFlags n `set` ChangedEnvForm }              -- Set envelope form 14 '^'
            | otherwise = getNotes n
                where
                    setCmd c = do
                        d <- lift getAsWord8
                        getNotes $ n{ noteCmd = c $ d }

        yieldNote x = do
            e <- if (noteEnvEnable x)
                    then lift $ toEnvFreq <$> getWord8
                    else return $ noteEnvFreq x
            yieldNote' $ x{noteEnvFreq = e}

        yieldNote' x = do
            r <- get
            ns <- getNotes $ x{noteCmd = NoteCmdNone, noteFlags = noFlags }
            return $ x : replicate r newNote ++ ns

putChannel :: Channel -> (Put,Int)
putChannel ch = execState (mapM_ doPutNote (packChannel ch) >> doModify1 255 ) (mempty, 0)
    where
        doPutNote :: (Note, Maybe Int) -> State (Put, Int) ()
        doPutNote (nN, mc) = do
            when (noteFlags nN `isSet` ChangedSample)
                $ doModifyM 0xa0 $ noteSample nN

            when (noteFlags nN `isSet` ChangedOrnament)
                $ doModifyM 0xc0 $ noteOrnament nN

            when (noteFlags nN `isSet` ChangedVolume && noteVolume nN /= 0)
                $ doModifyM 0xe0 $ noteVolume nN

            when (noteFlags nN `isSet` ChangedEnvEnable && noteEnvEnable nN)
                $ doModify1 0xe0

            when (noteFlags nN `isSet` ChangedNoise)
                $ doModifyC 0xf0 $ fromNoise $ noteNoise nN

            when (noteFlags nN `isSet` ChangedEnvForm && noteEnvForm nN /= EnvFormNone)
                $ doModify1 $ 0xf0 + (fromEnum $ noteEnvForm nN)

            maybe (return ()) (\c -> doModify1 $ 0x60 + c) mc

            when (noteCmd nN /= NoteCmdNone)
                $ putNoteCmd nN

            doModify1 $ putPitch $ notePitch nN

            when (noteEnvEnable nN &&  all (notePitch nN /=) [Release,Pause,NoNote])
                $ doModify1 $ noteEnvFreq nN

        putPitch Release = 0x5e
        putPitch Pause = 0x5f
        putPitch NoNote = 0x5d
        putPitch n = fromEnum n - fromEnum pitchAS0

        putNoteCmd nN = case noteCmd nN of
                             NoteCmdHldSample   -> doModify1 0xf1
                             NoteCmdHldImage    -> doModify1 0xf2
                             NoteCmdHldInstr    -> doModify1 0xf3
                             NoteCmdDelay     n -> doModifyC 0xf4 n
                             NoteCmdGlisUp _  n -> doModifyC 0xf5 n
                             NoteCmdGlisDn _  n -> doModifyC 0xf6 n
                             NoteCmdPortaR    n -> doModifyC 0xf7 n
                             NoteCmdPorta _ _ n -> doModifyC 0xf9 n
                             NoteCmdVolSlide  n -> doModifyC 0xfb $ intShrink 32 n
                             _ -> return ()

        doModify c f = modify $ \(p, l) -> (p >> f, l + c)

        doModify1 f = doModify 1 $ putAsWord8 f

        doModifyM b f = doModify1 (b + f)

        doModifyC c n = doModify 2 $ do
            putWord8 c
            putAsWord8 n


--------------
class InstrumentData d where
    getInstrumentData :: GetInstrumentData d
    putInstrumentData :: PutInstrumentData d
    sizeInstrumentData :: [d] -> Int
    showsInstrumentData :: d -> ShowS

type GetInstrumentData d = Int -> Get ([d], (Int, Int))
type PutInstrumentData d = Int -> Int -> Int -> Int -> d -> Put

getInstruments :: (InstrumentData d) => Int -> Get [Instrument d]
getInstruments s = do
    skip s
    forM [0..31] $ \i -> do
        w <- getWord16le
        (d,(ls,le)) <- lookAhead $ getInstrumentData (fromIntegral w - i * 2 - 2)
        return $ newInstrument
            { instrumentNumber = i
            , instrumentData = d
            , instrumentLoopStart = ls
            , instrumentLoopEnd = le
            }

putInstruments :: (InstrumentData d) => [Instrument d] -> (Put, Int)
putInstruments inss = (putOffsets 64 l >> p, 2 * length l + sum l)
    where
        p = mapM_ doPutID inss
        l = [sizeInstrumentData (instrumentData i) | i <- inss]
        doPutID i = do
            let ls = instrumentLoopStart i
            let le = instrumentLoopEnd i
            let inds = instrumentData i
            let lsd = length inds
            foldM_ (\c ind -> putInstrumentData c ls le lsd ind >> return (c + 1)) 0 inds



showInstrument :: (InstrumentData d) => String -> String -> Instrument d -> [String]
showInstrument title sep instr = padSRight (length sep) (title ++ shows32 (instrumentNumber instr) "")
                                    : sep
                                    : [ (shows2 i . (" | " ++). showChar is . showChar ie. (" | "++) . showsInstrumentData d $ "") |
                                        (i,d) <- zip [0..] $ instrumentData instr,
                                        let is = if i == instrumentLoopStart instr then '(' else ' ',
                                        let ie = if i == instrumentLoopEnd instr then ')' else ' ' ]
                                    ++ [sep]

--------------
getSamples :: Int -> Get [Sample]
getSamples = getInstruments

putSamples :: [Sample] -> (Put, Int)
putSamples = putInstruments

sampleSep = "---+----+-----+------------"
showASCSample = showInstrument "Sample: " sampleSep

--------------
instance InstrumentData SampleData where
    getInstrumentData = getSampleData
    putInstrumentData = putSampleData
    sizeInstrumentData = sizeSampleData
    showsInstrumentData = showsSampleData

showsSampleData :: SampleData -> ShowS
showsSampleData sd = showsSgnInt 3 (sampleDataNoise sd) . ( " | " ++ )
    . showsSgnInt 4 (sampleDataTone sd) . (' ':)
    . shows2 (sampleDataVolume sd) . (' ':)
    . showsTM . showsNM . showsSDE (sampleDataEffect sd)
    where
        showsTM = if sampleDataToneEnable sd then ('T':) else ('_':)
        showsNM = if sampleDataNoiseEnable sd then ('N':) else ('_':)

getSampleData :: GetInstrumentData SampleData
getSampleData s = do
    skip s
    runStateT (doGetSampleData 0) (0,0)
        where
            doGetSampleData :: Int -> StateT (Int, Int) Get [SampleData]
            doGetSampleData i = do
                b0 <- lift $ getInt8
                b1 <- lift $ getInt8
                b2 <- lift $ getWord8
                when (b0 `testBit` 7) $ modify (\(_,x) -> (i,x))
                when (b0 `testBit` 6) $ modify (\(x,_) -> (x,i))
                let nd = fromIntegral $ if b0 `testBit` 4 then b0 .|. -0x20 else b0 .&. 0x1f
                let td = fromIntegral $ b1
                let v =  fromIntegral $ b2 `shiftR` 4 .&. 0xf
                let tm = not $ b2 `testBit` 0
                let nm = not $ b2 `testBit` 3
                let ef = toSampleDataEffect $ fromIntegral $ b2 `shiftR` 1 .&. 3
                rest <-  if (testBit b0 5) then return [] else doGetSampleData (i+1)
                return $ (newSampleData { sampleDataNoise = nd,
                                          sampleDataTone = td,
                                          sampleDataVolume = v,
                                          sampleDataNoiseEnable = nm,
                                          sampleDataToneEnable = tm,
                                          sampleDataEffect = ef
                                          }) : rest

putSampleData :: PutInstrumentData SampleData
putSampleData i ls le lsd sd = do
            putWord8
                $ changeBit 7 (i == ls)
                $ changeBit 6 (i == le)
                $ changeBit 5 (i == lsd - 1)
                $ fromIntegral $ sampleDataNoise sd
            putAsWord8 $ sampleDataTone sd
            putWord8
                $ changeBit 0 (not $ sampleDataToneEnable sd)
                $ changeBit 3 (not $ sampleDataNoiseEnable sd)
                $ (fromSampleDataEffect $ sampleDataEffect sd) `shiftL` 1
                .|. (fromIntegral (sampleDataVolume sd) `shiftL` 4)

sizeSampleData :: [SampleData] -> Int
sizeSampleData sds = 3 * length sds

---------------
showsSDE :: SampleDataEffect -> ShowS
showsSDE = lookupChar '_' [(SDEUp, '+'), (SDEDown, '-'), (SDEEnv, 'E')]

toSampleDataEffect :: Int -> SampleDataEffect
toSampleDataEffect x = maybe SDENone id $ lookup x [(1, SDEEnv), (2, SDEDown), (3, SDEUp)]

fromSampleDataEffect :: SampleDataEffect -> Word8
fromSampleDataEffect sd = maybe 0 id $ lookup sd [(SDEEnv, 1), (SDEDown, 2), (SDEUp, 3)]

--------------
getImages :: Int -> Get [Ornament]
getImages = getInstruments

putImages :: [Ornament] -> (Put, Int)
putImages = putInstruments

imageSep = "---+----+-----+-----"
showASCImage = showInstrument "Image: " imageSep

--------------
instance InstrumentData OrnamentData where
    getInstrumentData = getImageData
    putInstrumentData = putImageData
    sizeInstrumentData = sizeImageData
    showsInstrumentData = showsImageData

showsImageData im = showsSgnInt 3 (ornamentDataNoise im) . ( " | " ++ ) . showsSgnInt 4 (ornamentDataTone im)

getImageData :: GetInstrumentData OrnamentData
getImageData s = do
    skip s
    runStateT (doGetImageData 0) (0,0)
        where
            doGetImageData :: Int -> StateT (Int, Int) Get [OrnamentData]
            doGetImageData i = do
                b0 <- lift $ getInt8
                b1 <- lift $ getInt8
                when (b0 `testBit` 7) $ modify (\(_,x) -> (i,x))
                when (b0 `testBit` 6) $ modify (\(x,_) -> (x,i))
                let nd = fromIntegral $ if b0 `testBit` 4 then b0 .|. -0x20 else b0 .&. 0x1f
                let td = fromIntegral $ b1
                rest <-  if (testBit b0 5) then return [] else doGetImageData (i+1)
                return $ (newOrnamentData { ornamentDataNoise = nd, ornamentDataTone = td } ) : rest

putImageData ::  PutInstrumentData OrnamentData
putImageData i ls le lsd od = do
    putWord8
        $ changeBit 7 (i == ls)
        $ changeBit 6 (i == le)
        $ changeBit 5 (i == lsd - 1)
        $ fromIntegral $ ornamentDataNoise od
    putAsWord8 $ ornamentDataTone od

sizeImageData :: [OrnamentData] -> Int
sizeImageData ids = 2 * length ids
