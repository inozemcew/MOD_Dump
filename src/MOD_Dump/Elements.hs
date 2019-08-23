{-# LANGUAGE PatternSynonyms #-}
module MOD_Dump.Elements
    ( Instrument, instrumentNumber, instrumentData, instrumentLoopStart, instrumentLoopEnd, newInstrument
    , Sample, sampleNumber, sampleData, sampleLoopStart, sampleLoopEnd, newSample
    , SampleData, sampleDataVolume, sampleDataToneMask, sampleDataNoiseMask, sampleDataTone, sampleDataNoise, sampleDataEffect, newSampleData
    , SampleDataEffect(SDENone, SDEEnv, SDEDown, SDEUp)
    , Ornament, ornamentNumber, ornamentData, newOrnament
    , OrnamentData, ornamentDataTone, ornamentDataNoise, newOrnamentData
    , Pitch(Pitch, Pause, Release, NoNote), pitchKey, pitchOctave, isPitch, pitchC1, pitchAS0
    , EnvFreq, toEnvFreq, noEnvFreq, EnvForm(..), noEnvForm
    , ChannelMask, channelMaskValue, noChannelMask
    , Noise, toNoise, noNoise
    , Position, newPosition, positionNumber, positionTranspose
    , NoteCmd(..)
    , Shared, sharedEnvForm, sharedEnvFreq, sharedMask,sharedNoise, newShared
    , Note, noteCmd, notePitch, noteSample, noteOrnament, noteVolume, noteEnvForm, noteEnvFreq, noteNoise, newNote
    , Channel
    , Row, rowNumber, rowShared, rowNotes, makeRows, makeRowsWithShared
    , Pattern, patternNumber, patternRows, newPattern
    , Tables, positionsTable, patternsTable, samplesTable, ornamentsTable, newTables
    , ModuleData, delay, loopingPos, positions, patterns, samples, ornaments, title, author, size, newModuleData
    ) where

import Data.List(transpose)

data ModuleData = AModuleData
    { delay :: Int
    , loopingPos :: Int
    , positions :: [Position]
    , patterns :: [Pattern]
    , samples :: [Sample]
    , ornaments :: [Ornament]
    , title :: String
    , author :: String
    , size :: Int
    } deriving (Eq)

newModuleData :: ModuleData
newModuleData = AModuleData 0 0 [] [] [] [] "" "" 0

data Tables = ATables
    { positionsTable :: Int
    , patternsTable :: Int
    , samplesTable :: Int
    , ornamentsTable :: Int
    } deriving (Show)

newTables :: Tables
newTables = ATables 0 0 0 0

data Position = APosition
    { positionNumber :: Int
    , positionTranspose :: Int
    } deriving (Eq)

instance Ord Position where
    x <= y = positionNumber x <= positionNumber y

newPosition :: Position
newPosition = APosition 0 0

-------------------------------
data Key = KeyC | KeyCS | KeyD  | KeyDS | KeyE | KeyF | KeyFS | KeyG  | KeyGS | KeyA | KeyAS | KeyB deriving (Eq, Show, Ord, Enum)

data Pitch = Pitch
    { pitchKey :: Key
    , pitchOctave :: Int
    } | Pause | Release | NoNote deriving (Eq)

instance Show Pitch where
    show (Pitch k o) = showChar '<' . showString (["C-","C#","D-","D#","E-","F-","F#","G","G#","A-","A#","B-"] !! fromEnum k) . shows o $ ">"
    show Pause = "<Pause>"
    show Release = "<Release>"
    show NoNote = "<None>"

instance Ord Pitch where
    (Pitch kx ox)  <= (Pitch ky oy)
        | ox /= oy = ox < oy
        | kx /= ky = kx < ky
    NoNote <= _ = True
    Pause <= x = (x /= NoNote)
    (Pitch _ _) <= _ = False
    Release <= (Pitch _ _) = True
    _ <= _ = False

instance Enum Pitch where
    toEnum x = let (o,k) = x `divMod` 12 in Pitch
                    { pitchOctave =  o
                    , pitchKey = toEnum k
                    }

    fromEnum (Pitch k o) = o * 12 + fromEnum k
    fromEnum NoNote = minBound + 1
    fromEnum Pause = minBound + 2
    fromEnum Release = minBound + 3

instance Bounded Pitch where
    minBound = Pitch KeyC (-1)
    maxBound = Pitch KeyB (10)

isPitch :: Pitch -> Bool
isPitch (Pitch _ _) = True
isPitch _ = False

pitchC1 = Pitch KeyC 1
pitchAS0 = Pitch KeyAS 0
---------------
type EnvFreq = Int

toEnvFreq :: (Integral a) => a -> EnvFreq
toEnvFreq = fromIntegral

noEnvFreq :: Int
noEnvFreq = 0

data EnvForm = EnvFormNone
             | EnvFormDecay
             | EnvFormAttack
             | EnvFormRepDecay
             | EnvFormRepDecayAttack
             | EnvFormDecaySus
             | EnvFormRepAttack
             | EnvFormAttackSus
             | EnvFormRepAttackDecay
               deriving (Eq, Show)

noEnvForm = EnvFormNone

instance Enum EnvForm where
    toEnum x | (x >= 0 && x <= 3) || x == 9  = EnvFormDecay
             | (x >= 4 && x <= 7) || x == 15 = EnvFormAttack
             | x == 8 = EnvFormRepDecay
             | x == 10 = EnvFormRepDecayAttack
             | x == 11 = EnvFormDecaySus
             | x == 12 = EnvFormRepAttack
             | x == 13 = EnvFormAttackSus
             | x == 14 = EnvFormRepAttackDecay
             | otherwise = EnvFormNone

    fromEnum x = case x of
                    EnvFormNone -> (-1)
                    EnvFormDecay -> 9
                    EnvFormAttack -> 15
                    EnvFormRepDecay -> 8
                    EnvFormRepDecayAttack -> 10
                    EnvFormDecaySus -> 11
                    EnvFormRepAttack -> 12
                    EnvFormAttackSus -> 13
                    EnvFormRepAttackDecay ->14

---------------
newtype ChannelMask = ChannelMask { channelMaskValue :: Int } deriving (Eq)

noChannelMask = ChannelMask 7

---------------
type Noise = Int

toNoise :: (Integral a) => a -> Noise
toNoise = fromIntegral

noNoise :: Noise
noNoise = 0

---------------
data NoteCmd = NoteCmdNone
             | NoteCmdHldSample
             | NoteCmdHldImage
             | NoteCmdHldInstr
             | NoteCmdDelay Int
             | NoteCmdGlisUp Int
             | NoteCmdGlisDn Int
             | NoteCmdPorta Int
             | NoteCmdPortaR Int
               deriving (Eq)

data Shared = AShared
    { sharedEnvFreq :: EnvFreq
    , sharedEnvForm :: EnvForm
    , sharedMask :: ChannelMask
    , sharedNoise :: Noise
    } deriving (Eq)

newShared :: Shared
newShared = AShared noEnvFreq noEnvForm noChannelMask noNoise

data Note = ANote
    { noteCmd :: NoteCmd
    , notePitch :: Pitch
    , noteSample :: Int
    , noteOrnament :: Int
    , noteVolume :: Int
    , noteEnvForm :: EnvForm
    , noteEnvFreq :: EnvFreq
    , noteNoise :: Noise
    } deriving (Eq)

newNote = ANote NoteCmdNone NoNote 0 0 15 EnvFormNone noEnvFreq noNoise

type Channel = [Note]


data Row = ARow
    { rowNumber :: Int
    , rowShared :: Shared
    , rowNotes :: [Note]
    } deriving (Eq)

makeRowsWithShared :: ([Note] -> Shared) ->  [Channel] -> [Row]
makeRowsWithShared f cs = map makeRow $ zip3 [0..] ss $ tcs
    where
        makeRow (i,s,rs) = ARow i s rs
        tcs = transpose $ cs
        ss = map f tcs

makeRows :: [Channel] -> [Row]
makeRows = makeRowsWithShared (const newShared)

data Pattern = APattern
    { patternNumber :: Int
    , patternRows :: [Row]
    } deriving (Eq)

newPattern :: Pattern
newPattern = APattern 0 []

instance Show Pattern where
    showsPrec _ pat = showString "Pattern " . shows (patternNumber pat)

-------------------------------
data Instrument d = AnInstrument
    { instrumentNumber :: Int
    , instrumentData :: [d]
    , instrumentLoopStart :: Int
    , instrumentLoopEnd :: Int
    } deriving (Eq, Show)

newInstrument :: Instrument d
newInstrument = AnInstrument 0 [] 0 0

---------------

type Sample = Instrument SampleData

pattern Sample {sampleNumber, sampleData, sampleLoopStart, sampleLoopEnd } = AnInstrument sampleNumber sampleData sampleLoopStart sampleLoopEnd
newSample = newInstrument
---------------

data SampleData = ASampleData
    { sampleDataNoise :: Int
    , sampleDataTone :: Int
    , sampleDataVolume :: Int
    , sampleDataNoiseMask :: Bool
    , sampleDataToneMask :: Bool
    , sampleDataEffect :: SampleDataEffect
    } deriving (Eq)

newSampleData = ASampleData 0 0 0 False False SDENone

data SampleDataEffect = SDENone | SDEEnv | SDEDown | SDEUp  deriving (Eq)

---------------
type Ornament = Instrument OrnamentData

pattern Ornament {ornamentNumber, ornamentData, ornamentLoopStart, ornamentLoopEnd } = AnInstrument ornamentNumber ornamentData ornamentLoopStart ornamentLoopEnd

newOrnament = newInstrument

---------------
data OrnamentData = AnOrnamentData
    { ornamentDataNoise :: Int
    , ornamentDataTone :: Int
    } deriving (Eq)

newOrnamentData :: OrnamentData
newOrnamentData = AnOrnamentData 0 0

