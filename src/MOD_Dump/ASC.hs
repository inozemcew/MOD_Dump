module MOD_Dump.ASC (readASCModule)  where

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
    -- +(`Sample table offset`) = Samples' offsets :: [Word]::64
    -- +(`Sample table offset`) + (`Sample offset`) = Sample data
    --
    -- +(`Image table offset`) = Images' offsets :: [Word]::64
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
        --                  | 0xfb, n      = Unknown command, consumes 1 byte
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



import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import GHC.Word
import Data.List(intercalate, transpose)

readASCModule :: String -> B.ByteString -> Maybe Module
readASCModule e bs = do
    guard (e == ".asc" || e ==".C")
    m <- either (const Nothing) (\(_,_,x) -> Just x) $ runGetOrFail getASCModule bs
    return $ ascModule m

ascModule :: ASCModule -> Module
ascModule m = Module {
    printInfo =  printASCInfo m,
    printPatterns = \w r  -> mapM_ printASCPatterns $ splitBy w $ filterInRange patternNumber r $ patterns m,
    printSamples = \r  -> mapM_ printASCSamples $ splitBy 3 $ filterInRange sampleNumber r  $ samples m,
    printOrnaments = \r  -> mapM_ printASCImages $ splitBy 3 $ filterInRange imageNumber r  $ images m
}

---------------
data ASCModule = ASCModule {
        delay :: Int,
        loopingPos :: Int,
        positions :: [Position],
        patterns :: [Pattern],
        samples :: [Sample],
        images :: [Image],
        title :: String,
        author :: String
    } deriving (Eq, Show)

data Tables = Tables {
    patternsTable :: Int,
    sampleTable :: Int,
    imageTable :: Int
} deriving (Show)

getASCModule :: Get ASCModule
getASCModule = do
            -- for module with built-in player get Title and Author from that player and skip to module data
            ta <- lookAheadM getMaybeTandAFromPlayer

            (d, l, tables, ps, t, a) <- lookAhead $ getHeader ta
            -- sample table can start only 9 or 72 bytes after positions table
            guard $ (patternsTable tables - length ps ==) `any` [9,72]

            s1 <- peekWord16 $ sampleTable tables
            guard (s1 == 0x40)

            i1 <- peekWord16 $ imageTable tables
            guard (i1 == 0x40)

            let count = maximum ps
            p1 <- peekWord16 $ patternsTable tables
            guard (count * 6 + 6 == p1)

            patterns' <- lookAhead $ getPatterns (patternsTable tables) count

            samples' <- lookAhead $ getSamples (sampleTable tables)

            images' <- getImages (imageTable tables)

            return ASCModule {
                delay =  d,
                loopingPos = l,
                positions = ps,
                patterns = patterns',
                samples = samples',
                images = images',
                title = t,
                author = a
            }

getHeader :: Maybe (String, String) -> Get (Int, Int, Tables, [Position], String, String)
getHeader ta =  do
        d <- fromIntegral `liftM` getWord8
        l <- fromIntegral `liftM` getWord8

        p <- fromIntegral `liftM` getWord16le
        s <- fromIntegral `liftM` getWord16le
        o <- fromIntegral `liftM` getWord16le
        let tables = Tables p s o

        n <- fromIntegral `liftM` getWord8
        ps <- replicateM n $ fromIntegral `liftM` getWord8

        (t,a) <- maybe (getTAndA p ps) return ta
        return (d, l, tables, ps, t, a)

getTAndA :: Int -> [Int] -> Get (String, String)
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


printASCInfo :: ASCModule -> IO ()
printASCInfo m = do
    putStrLn $ "Song type: ASC Sound master compiled song"
    putStrLn $ "Song title: " ++ show (title m)
    putStrLn $ "Composed: " ++ show (author m)
    putStrLn $ "Delay: " ++ show (delay m)
    putStrLn $ "Looping position: " ++ show (loopingPos m)

    putStrLn $ "Positions: " ++ concatMap showPosition (positions m)

-----------------------------------------------

type Position = Int
showPosition pn = '{':show pn ++ "}"

---------------
data Pattern = Pattern Int [Shared] Channel Channel Channel deriving (Eq)

instance Show Pattern where
    showsPrec _ pat = showString "Pattern " . shows (patternNumber pat)

patternNumber :: Pattern -> Int
patternNumber (Pattern n _ _ _ _) = n

mapPattern :: ((Int, Shared, [Note]) -> a) -> Pattern -> [a]
mapPattern f (Pattern _ ss as bs cs) = map f $ zip3 [0..] ss $ transpose [as,bs,cs]

showRow :: (Int, Shared, [Note]) -> String
showRow (i, (Shared freq form mask noise), abc) = let header = shows2 i . (" | " ++). shows freq . shows form . shows mask . shows noise . (' ':)
                                                  in  foldl (\a x -> a . shows x) header abc ""

showPattern :: Pattern -> [String]
showPattern p = padSRight (length patternSep) (show p) : patternSep : mapPattern (showRow) p ++ [patternSep, ""]

patternSep = "---+--------+------------+------------+----------- "

getPatterns:: Int -> Int -> Get [Pattern]
getPatterns offset count = do
    skip offset
    forM [0..count-1] $ \i -> do
        a <- getWord16le
        b <- getWord16le
        c <- getWord16le
        let i6 = i * 6 + 6
        chA <-lookAhead $ getChannel (fromIntegral a - i6)
        chB <-lookAhead $ getChannel (fromIntegral b - i6)
        chC <-lookAhead $ getChannel (fromIntegral c - i6)
        return $ Pattern i (map shared $ transpose [chA, chB, chC]) chA chB chC

printASCPatterns :: [Pattern] -> IO ()
printASCPatterns ps = printColumned (length patternSep) $ map (showPattern) ps

-------------------------------------------------------------

data Pitch = Pitch Int | Pause | Release | None deriving (Eq)

instance Show Pitch where
    showsPrec _ Pause = showString "PSE"
    showsPrec _ Release = showString "RLS"
    showsPrec _ None = showString "___"
    showsPrec _ (Pitch i) = let (o,n) = divMod (i+10) 12 in  showChar (octaveNames !! o) . showString ( noteNames !! n)
        where
            noteNames = ["_C","#C","_D","#D","_E","_F","#F","_G","#G","_A","#A","_B"]
            octaveNames = "UCLS1234"

---------------
data NoteCmd = NoCmd | HldSample | HldImage | HldInstr | Quant Int | GlisUp Int | GlisDn Int | PortM Int | PortP Int deriving (Eq)

instance Show NoteCmd where
    showsPrec _ NoCmd = showChar '|'
    showsPrec _ _     = showChar '#'

---------------
newtype EnvFreq = EnvFreq Int deriving (Ord,Eq)

instance Show EnvFreq where
    showsPrec _ (EnvFreq x) = if x == 0 then ("___" ++) else shows3 x

toEnvFreq :: (Integral a) => a -> EnvFreq
toEnvFreq = EnvFreq . fromIntegral

noEnvFreq = EnvFreq 0

data EnvForm = EnvFormNone | EnvFormDecay | EnvFormDecayAttack | EnvFormAttack | EnvFormAttackDecay deriving (Eq)

instance Show EnvForm where
    showsPrec _ EnvFormDecay = showChar '\\'
    showsPrec _ EnvFormDecayAttack = showChar 'V'
    showsPrec _ EnvFormAttack = showChar '/'
    showsPrec _ EnvFormAttackDecay = showChar '^'
    showsPrec _ _ = showChar '_'


newtype NoiseMask = NoiseMask Int deriving (Eq)

noNoiseMask = NoiseMask 7

instance Show NoiseMask where
    showsPrec _ (NoiseMask 7) = ('_':)
    showsPrec _ (NoiseMask x) = showsB 1 x

maskShift1 :: NoiseMask -> NoiseMask
maskShift1 (NoiseMask m) = NoiseMask (m `div` 2 + 4)
maskShift0 :: NoiseMask -> NoiseMask
maskShift0 (NoiseMask m) = NoiseMask (m `div` 2)

newtype Noise = Noise Int deriving (Eq)

noNoise = Noise 0

instance Show Noise where
    showsPrec _ (Noise 0) = ('_':)
    showsPrec _ (Noise x) = shows32 x

toNoise :: (Integral a) => a -> Noise
toNoise = Noise . fromIntegral

---------------
data Shared = Shared {
    sharedEnvFreq :: EnvFreq,
    sharedEnvForm :: EnvForm,
    sharedMask :: NoiseMask,
    sharedNoise :: Noise
} deriving (Eq)

instance Show Shared where
    showsPrec _ (Shared freq form mask noise) = shows freq . shows form .shows mask. shows noise

shared :: [Note] -> Shared
shared abc = Shared {
    sharedEnvFreq = eFreq,
    sharedEnvForm = eForm,
    sharedMask = mask,
    sharedNoise = noise
}
    where
        findF f a x = if noteVolume x == 0 then f x else a
        eFreq = foldl (findF noteEnvFreq) noEnvFreq abc
        eForm = foldl (findF noteEnvForm) EnvFormNone abc
        (noise,mask) = foldl (\(n,m) x -> let nn = noteNoise x in if nn == noNoise then (n, maskShift1 m) else (nn, maskShift0 m)) (noNoise, noNoiseMask) abc

---------------
data Note = Note {
    noteCmd :: NoteCmd,
    notePitch :: Pitch,
    noteSample :: Int,
    noteImage :: Int,
    noteVolume :: Int,
    noteEnvForm :: EnvForm,
    noteEnvFreq :: EnvFreq,
    noteNoise :: Noise
} deriving (Eq)

instance Show Note where
    showsPrec _ (Note cmd (Pitch pitch) sample image volume _ _ _) = shows cmd
        .(' ':). shows (Pitch pitch)
        .(' ':). shows32 sample
        .(' ':). shows32 image
        .(' ':). showsVolume volume
        .(' ':)
        where
            showsVolume v
                | v == 0 = ("EN" ++)
                | otherwise = shows2 v

    showsPrec _ note = shows (noteCmd note) .(' ':). shows (notePitch note) .(" _ _ __ "++)

---------------
type Channel = [Note]

getChannel :: Int -> Get Channel   --- Empty channel returns empty list - error!!
getChannel offset = skip offset >> evalStateT getNewNotes 0
    where
        emptyNote = Note NoCmd None 0 0 15 EnvFormNone noEnvFreq noNoise
        getNewNotes = getNotes emptyNote
        getNotes n = do
            v <- lift getWord8
            switch n $ fromIntegral v

        switch n x
            | x == 255 = return []                                              -- End of pattern
            | x <= 0x55 = yieldNote $ n{notePitch = Pitch x}                    -- Note, may be followed by env period if volume='EN'
            | x >= 0x56 && x <= 0x5d = yieldNote' $ n{notePitch = None}         -- No note, just placeholder for empty channel
            | x == 0x5e = yieldNote' $ n{notePitch = Release}                   -- Release sample
            | x == 0x5f = yieldNote' $ n{notePitch = Pause}                     -- Pause
            | x >= 0x60 && x <= 0x9f = do                                       -- Skip x rows after note or relese or pause
                put (x - 0x60)
                getNotes n
            | x >= 0xa0 && x <= 0xbf = getNotes $ n{noteSample = x - 0xa0}      -- Set current sample
            | x >= 0xc0 && x <= 0xdf = getNotes $ n{noteImage = x - 0xc0}       -- Set current image
            | x >= 0xe0 && x <= 0xef = getNotes $ n{noteVolume = x - 0xe0}      -- Set current volume
            | x == 0xf0 = do                                                    -- Set noise period
                noise <- lift getWord8
                getNotes $ n{noteNoise = toNoise noise}
            | x == 0xf1 = getNotes $ n{noteCmd = HldSample}                     -- Hold sample command
            | x == 0xf2 = getNotes $ n{noteCmd = HldImage}                      -- Hold image command
            | x == 0xf3 = getNotes $ n{noteCmd = HldInstr}                      -- Hold sample and image
            | x == 0xf4 = setCmd Quant                                          -- Set delay command
            | x == 0xf5 = setCmd GlisUp                                         -- GlisUp command
            | x == 0xf6 = setCmd GlisDn                                         -- GlisDn command
            | x == 0xf7 = setCmd PortP                                          -- Port.S+ command
            | x == 0xf8 = getNotes $ n{noteEnvForm = EnvFormDecay}              -- Set envelope form 8 '\'
            | x == 0xf9 = setCmd PortM                                          -- Port.S- command
            | x == 0xfa = getNotes $ n{noteEnvForm = EnvFormDecayAttack}        -- Set envelope form 10 'V'
            | x == 0xfb = lift getWord8 >> getNotes n                           -- Unknown command, consumes 1 byte
            | x == 0xfc = getNotes $ n{noteEnvForm = EnvFormAttack}             -- Set envelope form 12 '/'
            | x == 0xfe = getNotes $ n{noteEnvForm = EnvFormAttackDecay}        -- Set envelope form 14 '^'
            | otherwise = getNotes n
            where
                setCmd c = do
                    d <- lift getWord8
                    getNotes $ n{noteCmd = c $ fromIntegral d}

        yieldNote x = do
            e <- if (noteVolume x == 0)
                    then lift $ toEnvFreq `liftM` getWord8
                    else return $ noteEnvFreq x
            yieldNote' $ x{noteEnvFreq = e}

        yieldNote' x = do
            r <- get
            ns <- getNotes $ x{noteCmd = NoCmd}
            return $ x : replicate r emptyNote ++ ns

--------------
data (InstrumentData d) => Instrument d = Instrument {
    instrumentNumber :: Int,
    instrumentData :: [d],
    instrumentLoopStart :: Int,
    instrumentLoopEnd :: Int
} deriving (Eq, Show)

getInstruments :: (InstrumentData d) => Int -> Get [Instrument d]
getInstruments s = do
    skip s
    forM [0..31] $ \i -> do
        w <- getWord16le
        (d,(ls,le)) <- lookAhead $ getInstrumentData (fromIntegral w - i * 2 - 2)
        return $ Instrument i d ls le

showInstrument :: (InstrumentData d) => String -> String -> Instrument d -> [String]
showInstrument title sep instr = (title ++ shows32 (instrumentNumber instr) "")
                                    : sep
                                    : [ (shows2 i . (" | " ++). showChar is . showChar ie. (" | "++) $ show d) |
                                        (i,d) <- zip [0..] $ instrumentData instr,
                                        let is = if i == instrumentLoopStart instr then '(' else ' ',
                                        let ie = if i == instrumentLoopEnd instr then ')' else ' ' ]
                                    ++ [sep]

printInstruments :: (InstrumentData d) => String -> String -> [Instrument d] -> IO ()
printInstruments title sep is = printColumned (length sep) $ map (showInstrument title sep) is

class Show d => InstrumentData d where
    getInstrumentData :: Int -> Get ([d], (Int, Int))

--------------
type Sample = Instrument SampleData
sampleNumber = instrumentNumber
sampleData = instrumentData :: Sample -> [SampleData]
sampleLoopStart = instrumentLoopStart :: Sample -> Int
sampleLoopEnd = instrumentLoopEnd :: Sample -> Int

getSamples :: Int -> Get [Sample]
getSamples = getInstruments

sampleSep = "---+----+-----+------------"
printASCSamples :: [Sample] -> IO ()
printASCSamples = printInstruments "Sample: " sampleSep

--------------
data SampleData = SampleData {
    sampleDataNoiseDev :: Int,
    sampleDataToneDev :: Int,
    sampleDataVolume :: Int,
    sampleDataNoiseMask :: Bool,
    sampleDataToneMask :: Bool,
    sampleDataEffect :: SampleDataEffect
} deriving (Eq)

instance Show SampleData where
    showsPrec _ (SampleData nd td v nm tm ef) = showsSgnInt 3 nd . ( " | " ++ ) . showsSgnInt 4 td . (' ':) . shows2 v. (' ':) . showsTM . showsNM . shows ef
        where
            showsTM = if tm then ('T':) else ('_':)
            showsNM = if nm then ('N':) else ('_':)

instance InstrumentData SampleData where
    getInstrumentData = getSampleData

getSampleData :: Int -> Get ([SampleData], (Int, Int))
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
                return $ (SampleData nd td v nm tm ef) : rest

data SampleDataEffect = SDENone | SDEUp | SDEDown | SDEEnv deriving (Eq)

instance Show SampleDataEffect where
    showsPrec _ SDENone = showChar '_'
    showsPrec _ SDEUp = showChar '+'
    showsPrec _ SDEDown = showChar '-'
    showsPrec _ SDEEnv = showChar 'E'

toSampleDataEffect :: Int -> SampleDataEffect
toSampleDataEffect 1 = SDEEnv
toSampleDataEffect 2 = SDEDown
toSampleDataEffect 3 = SDEUp
toSampleDataEffect _ = SDENone

--------------
type Image = Instrument ImageData
imageNumber = instrumentNumber :: Image -> Int
imageData = instrumentData :: Image -> [ImageData]
imageLoopStart = instrumentLoopStart :: Image -> Int
imageLoopEnd = instrumentLoopEnd :: Image -> Int

getImages :: Int -> Get [Image]
getImages = getInstruments

imageSep = "---+----+-----+-----"
printASCImages :: [Image] -> IO ()
printASCImages = printInstruments "Image: " imageSep

--------------
data ImageData = ImageData {
    imageDataNoiseDev :: Int,
    imageDataToneDev :: Int
} deriving (Eq)

instance Show ImageData where
    showsPrec _ (ImageData nd td) = showsSgnInt 3 nd . ( " | " ++ ) . showsSgnInt 4 td

instance InstrumentData ImageData where
    getInstrumentData = getImageData

getImageData :: Int -> Get ([ImageData], (Int, Int))
getImageData s = do
    skip s
    runStateT (doGetImageData 0) (0,0)
        where
            doGetImageData :: Int -> StateT (Int, Int) Get [ImageData]
            doGetImageData i = do
                b0 <- lift $ getInt8
                b1 <- lift $ getInt8
                when (b0 `testBit` 7) $ modify (\(_,x) -> (i,x))
                when (b0 `testBit` 6) $ modify (\(x,_) -> (x,i))
                let nd = fromIntegral $ if b0 `testBit` 4 then b0 .|. -0x20 else b0 .&. 0x1f
                let td = fromIntegral $ b1
                rest <-  if (testBit b0 5) then return [] else doGetImageData (i+1)
                return $ (ImageData nd td ) : rest


