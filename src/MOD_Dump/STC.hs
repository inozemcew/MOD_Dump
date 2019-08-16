module MOD_Dump.STC (readSTCModule) where

import MOD_Dump.Module
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary.Get
import Data.Bits
import Data.List(intercalate)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State


readSTCModule :: FilePath -> B.ByteString -> Maybe Module
readSTCModule = readModule getSTCModule stcModule [".stc"]

data STCModule = STCModule  {
        delay :: Int,
        positions :: [Position],
        ornaments :: [Ornament],
        patterns :: [Pattern],
        identifier :: String,
        size :: Int,
        samples :: [Sample]
    } deriving (Eq, Show)

stcModule :: STCModule -> Module
stcModule m = Module {
        showInfo = showSTCModHeader m
        ,showPatterns   = \rs -> [ showPattern p | p <- patterns m, isInRanges rs $ patternNumber p ]
        ,showSamples    = \rs -> [ showSample s | s <- samples m, isInRanges rs $ sampleNumber s ]
        ,showOrnaments  = \rs -> [ showOrnament o | o <- ornaments m, isInRanges rs $ ornNumber o ]
    }

data Tables = Tables {
    positionsTable :: Int,
    ornamentsTable :: Int,
    patternsTable  :: Int
} deriving (Show)

getSTCModule :: Get STCModule
getSTCModule = do
    (delay', tables, identifier', size') <- getHeader

    let samplesCount = fromIntegral (minimum [f tables | f <- [positionsTable, ornamentsTable, patternsTable]] - 27) `div` 99
    samples'    <- getSamples samplesCount

    positions'  <- lookAhead $ skipTo (positionsTable tables) >> getPositions

    let ornamentsCount = fromIntegral ((patternsTable tables - ornamentsTable tables) `div` 33)
    ornaments'  <- lookAhead $ skipTo (ornamentsTable tables) >> getOrnaments ornamentsCount

    patterns'   <- skipTo (patternsTable tables) >> getPatterns

    return STCModule {
        delay = delay',
        positions = positions',
        ornaments = ornaments',
        patterns = patterns',
        identifier = identifier',
        size = size',
        samples = samples'
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
                return $ Tables (fromIntegral pos) (fromIntegral orn) (fromIntegral pat)


showSTCModHeader :: STCModule -> [String]
showSTCModHeader m =
    "Song type: Sound Tracker compiled song"
    : ("Song name: " ++ show (identifier m))
    : ("Delay: " ++ show (delay m))
    : [shows (f m) s | (f,s) <- [(length.positions," positions, "), (length.patterns, " patterns, "), (length.samples, " samples, "), (length.ornaments, " ornaments.")] ]
    ++ ["", "Positions: " ++ concatMap show (positions m)]

-----------------------------------------------------------------------------

data Ornament = Ornament {
        ornNumber :: Int,
        ornData :: [Int]
    } deriving (Eq, Show)

getOrnaments :: Int -> Get [Ornament]
getOrnaments ornamentsCount = replicateM ornamentsCount getOrnament
    where
        getOrnament = do
            l <- fromIntegral `liftM` getWord8
            ds <- replicateM 32 $ fromIntegral `liftM` getInt8
            return $ Ornament l ds

showOrnament :: Ornament -> [String]
showOrnament orn = let o = concat [showsSgnInt 3 i " " |  i <- ornData orn] in [padSRight (length o) $ "Ornament: " ++ (show $ ornNumber orn), o]

-----------------------------------------------------------------------------

data Sample = Sample {
        sampleNumber :: Int,
        sampleData :: [SampleData],
        sampleRepeatPos :: Int,
        sampleRepeatLen :: Int
    } deriving (Eq, Show)

getSamples :: Int -> Get [Sample]
getSamples samplesCount = replicateM samplesCount getSample
    where
        getSample = do
            n <- fromIntegral `liftM` getWord8
            d <- replicateM 32 getSampleData
            p <- fromIntegral `liftM` getWord8
            l <- fromIntegral `liftM` getWord8
            return $ Sample n d p l

showSample :: Sample -> [String]
showSample s = (padSRight width32 $ "Sample: " ++ show (sampleNumber s))
    : replicate width32 '-'
    : showSampleData (sampleData s)
    ++ [footer (sampleRepeatPos s) (sampleRepeatLen s)]
        where
            width = 3
            width32 = width * 32
            footer 0 _ = replicate (width*32) '-'
            footer start len = if start + len > 32 then out (start + len - 32) (32 - len) (32 - start) '#' '-'
                                                   else out (start - 1) len (33 - start - len) '-' '#'
            out n1 n2 n3 c1 c2 = replicate (width*n1) c1 ++ replicate (width*n2) c2 ++ replicate (width*n3) c1

---------------
data SampleData = SampleData {
        volume :: Int,
        noiseMask :: Bool,
        toneMask :: Bool,
        toneSlide :: Int,
        noise :: Int
    } deriving (Eq)

instance Show SampleData where
    showsPrec _ (SampleData v nm tm s n) = showsHex 1 v . showsMask 'T' tm . shows s. showsMask 'N' nm . showsHex 2 n
        where
            showsMask c m = showChar $ if m then c else '.'

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
    return $ SampleData v nm tm s n

showSampleData :: [SampleData] -> [String]
showSampleData ss = [concat [showVolume s i |   s <- ss ] | i <- [1..15]]
                 ++ [(concatMap (\s -> [' ', showMask 'T' (toneMask s), showMask 'N' (noiseMask s)]) ss)]
                 ++ [(foldr (\x -> showChar ' ' . showsHex 2 x) "" $ map noise ss)]
                    where
                        showMask c m = if m then c else '.'
                        showVolume s i = if volume s >= 16 - i then "(*)" else "..."

-----------------------------------------------------------------------------

data Position = Position {
        transposition :: Int,
        pnum :: Int
    } deriving (Eq)

instance Show Position where
    showsPrec _ (Position t pn) = let
            tp = if t == 0 then id else (('+':) . shows t)
        in ('{':) . shows pn . tp . ('}':)

getPositions :: Get [Position]
getPositions = do
    n <- getWord8
    replicateM (fromIntegral n) $ do
        pn <- getWord8
        t <- getWord8
        return $ Position (fromIntegral t) (fromIntegral pn)

----------------------------------------------------------------------------

data Pattern = Pattern Int [Note] [Note] [Note] deriving (Eq)

instance Show Pattern where
    showsPrec _ pat = showString "Pattern " . shows (patternNumber pat)
    
patternNumber :: Pattern -> Int
patternNumber (Pattern n _ _ _) = n

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
            return $ Pattern n chA chB chC


mapPattern :: ((Note, Note, Note) -> a) -> Pattern -> [a]
mapPattern _ (Pattern _ [] [] []) = []
mapPattern f (Pattern n (a:as) (b:bs) (c:cs)) = f (a, b, c) : mapPattern f (Pattern n as bs cs)

showRow :: (Note, Note, Note) -> String
showRow (a, b, c) = shows a . (" | " ++) . shows b . (" | " ++) $ show c

showPattern :: Pattern -> [String]
showPattern p = padSRight (length patternSep) (show p) : patternSep : mapPattern (showRow) p ++ [patternSep,""]

patternSep = "---------+----------+---------"

-- showSTCPatterns :: [Pattern] -> [String]
-- showSTCPatterns ps = showColumned $ map (showPattern) ps
-----------------------------------------------------------------------------
data Pitch = Pitch Int | Mute | None deriving (Eq)

instance Show Pitch where
    showsPrec _ Mute = showString "R--"
    showsPrec _ None = showString "---"
    showsPrec _ (Pitch p) = let (o,n) = divMod p 12 in showString ( noteNames !! n) . shows (o+1)

noteNames :: [String]
noteNames = ["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"]

-----------------------------------------------------------------------------

data Note = Note {
        pitch :: Pitch,
        sample :: Int,
        ornament :: Int,
        envelope :: Int
    } deriving (Eq)

instance Show Note where
    showsPrec _ (Note None 0 0 0) = shows None . showString " ----"
    showsPrec _ (Note Mute 0 0 0) = shows Mute . showString " ----"
    showsPrec _ (Note p s o e) = shows p . (' ':) . showsHex 1 s . showsHex 1 o . showsHex 2 e

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
                    | x <= 0x5f = yieldNote $ n{pitch = Pitch x}
                    | x == 0x80 = yieldNote $ n{pitch = Mute}
                    | x == 0x81 = yieldNote $ n{pitch = None}
                    | x >= 0x60 && x <= 0x6f = getNotes $ n{sample = x - 0x60}
                    | x >= 0x70 && x <= 0x7f = getNotes $ n{ornament = x - 0x70}
                    | x >= 0x83 && x <= 0x8e = do
                        o <- lift getWord8
                        getNotes $ n{envelope = x - 0x80, ornament = fromIntegral o}
                    | x >= 0xa1 && x <= 0xfe = do
                        put (x - 0xa1)
                        getNotes n

        getNewNotes = getNotes emptyNote

        emptyNote = Note None 0 0 0

        yieldNote x = do
            r <- get
            ns <- getNewNotes
            return $ x : replicate r emptyNote ++ ns

-----------------------------------------------------------------------------
