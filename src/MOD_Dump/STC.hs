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
readSTCModule e bs = do
    guard (e == ".stc")
    m <- either (const Nothing) (\(_,_,x) -> Just x) $ runGetOrFail getSTCModule bs
    return $ stcModule m

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
        printInfo = printSTCModHeader m
        ,printPatterns = \w r -> mapM_ printSTCPatterns $ splitBy w $ filterInRange patternNumber r $ patterns m
        ,printSamples = \r -> mapM_ printSTCSample $ filterInRange sampleNumber r $ samples m
        ,printOrnaments = \r -> mapM_ printSTCOrnament $ filterInRange ornNumber r $ ornaments m
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


printSTCModHeader :: STCModule -> IO ()
printSTCModHeader m = do
    putStrLn $ "Song type: Sound Tracker compiled song"
    putStrLn $ "Song name: " ++ show (identifier m)
    putStrLn $ "Delay: " ++ show (delay m)
    forM_ [(length.positions," positions, "), (length.patterns, " patterns, "), (length.samples, " samples, "), (length.ornaments, " ornaments.")] $
         \(f, s) -> putStr $ shows (f m) s
    putStrLn ""
    putStrLn $ "Positions: " ++ concatMap show (positions m)

-----------------------------------------------------------------------------

data Ornament = Ornament {
        ornNumber :: Int,
        ornData :: [Int]
    } deriving (Eq, Show)

getOrnaments :: Int -> Get [Ornament]
getOrnaments ornamentsCount = replicateM ornamentsCount getOrnament
    where
        getOrnament = do
            l <- getWord8
            ds <- replicateM 32 getInt8
            return $ Ornament (fromIntegral l) (map fromIntegral ds)

printSTCOrnament :: Ornament -> IO ()
printSTCOrnament orn = do
    putStrLn $ "Ornament: " ++ (show $ ornNumber orn)
    forM_ (ornData orn) $ \i -> do
        putStr $ showSgnInt 4 i
    putStrLn ""
    putStrLn ""

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
            n <- getWord8
            d <- replicateM 32 getSampleData
            p <- getWord8
            l <- getWord8
            return $ Sample (fromIntegral n) d (fromIntegral p) (fromIntegral l)

printSTCSample :: Sample -> IO ()
printSTCSample s = do
    putStrLn $ "Sample: " ++ show (sampleNumber s) 
    putStrLn $ replicate (width*32) '-'
    printSampleData $ sampleData s
    putStrLn $ footer (sampleRepeatPos s) (sampleRepeatLen s)
    putStrLn ""
        where
            width = 3
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

printSampleData :: [SampleData] -> IO ()
printSampleData ss = do
    forM_ [1..15] $ \i -> do
        forM_ ss $ \s -> putStr $ if volume s >= 16 - i then "(*)" else "..."
        putStrLn ""
    putStrLn $ concatMap (\s -> ' ' : showMask 'T' (toneMask s) : showMask 'N' (noiseMask s) : "") ss
    putStrLn $ foldr (\x -> showChar ' ' . showsHex 2 x) "" $ map noise ss
        where
            showMask c m = if m then c else '.'

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
showPattern p = take (length sep) (show p ++ repeat ' ') : sep : mapPattern (showRow) p ++ [sep,""]
    where 
        sep = "---------+----------+---------"

printSTCPatterns :: [Pattern] -> IO ()
printSTCPatterns ps = printLine $ map (showPattern) ps
    where
        printLine [] = putStrLn ""
        printLine l = do
            let h = intercalate "   " $ concatMap (take 1) l
            let t = map (drop 1) l
            putStrLn h
            if h == [] then return () else printLine t

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

--readNotes :: (Integral i) => i -> B.ByteString -> [Note]
--readNotes addr bs = runGet (evalStateT getNewNotes 0) $ B.drop (fromIntegral addr) bs
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
