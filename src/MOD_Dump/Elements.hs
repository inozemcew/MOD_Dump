{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MOD_Dump.Elements
    ( Instrument, instrumentNumber, instrumentData, instrumentLoopStart, instrumentLoopEnd, newInstrument, isEmptyInstrument
    , Sample, sampleNumber, sampleData, sampleLoopStart, sampleLoopEnd, newSample
    , SampleData, sampleDataVolume, sampleDataToneEnable, sampleDataNoiseEnable, sampleDataToneHold, sampleDataNoiseHold, sampleDataEnvEnable, sampleDataTone, sampleDataNoise, sampleDataEffect, newSampleData
    , SampleDataEffect(SDENone, SDEEnv, SDEDown, SDEUp)
    , Ornament, ornamentNumber, ornamentData, ornamentLoopStart, ornamentLoopEnd, newOrnament
    , OrnamentData, ornamentDataTone, ornamentDataNoise, newOrnamentData
    , Pitch(Pitch, Pause, Release, NoNote), pitchKey, pitchOctave, isPitch, pitchC1, pitchAS0
    , EnvFreq, toEnvFreq, noEnvFreq, EnvForm(..), noEnvForm
    , ChannelMask, channelMaskValue, noChannelMask
    , Noise, toNoise, fromNoise, noNoise
    , Position, newPosition, positionNumber, positionTranspose
    , NoteCmd(..)
    , Shared, sharedEnvForm, sharedEnvFreq, sharedMask,sharedNoise, newShared
    , Note(..), newNote
    , Channel, packChannel
    , Row, rowNumber, rowShared, rowNotes, makeRows, makeRowsWithShared, channelsFromRows
    , Pattern, patternNumber, patternRows, newPattern
    , Tables, positionsTable, patternsTable, samplesTable, ornamentsTable, newTables
    , ModuleData(..), newModuleData
    , Changed(..)
    ) where

import Data.List(transpose, intercalate, groupBy, mapAccumL)
import MOD_Dump.Utils
import Control.Monad.State
import MOD_Dump.FlagSet

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
    , mtype:: String
    , freqTType:: Maybe FreqTableType
    , songEnd :: Int } deriving (Eq)

instance Show ModuleData where
    showsPrec _ m = showString "({" . ("delay = " ++) . shows (delay m)
        . (", loopingPos = " ++) . shows (loopingPos m)
        . (", positions = (" ++) . shows (length $ positions m)
        . ("), patterns = (" ++) . shows (length $ patterns m)
        . ("), samples = (" ++) . shows (length $ samples m)
        . ("), ornaments = (" ++) . shows (length $ ornaments m)
        . ("), type = " ++) . shows (mtype m)
        . (", title = " ++) . shows (title m)
        . (", author = " ++) . shows (author m)
        . (", size = " ++) . shows (size m) . showString "},"
        . shows (positions m) . showString ")"

newModuleData :: ModuleData
newModuleData = AModuleData 0 0 [] [] [] [] "" "" 0 "" Nothing 0

data Tables = ATables
    { positionsTable :: Int
    , patternsTable :: Int
    , samplesTable :: Int
    , ornamentsTable :: Int } deriving (Show)

newTables :: Tables
newTables = ATables 0 0 0 0

data Position = APosition
    { positionNumber :: Int
    , positionTranspose :: Int } deriving (Eq)

instance Ord Position where
    x <= y = positionNumber x <= positionNumber y

instance Show Position where
    showsPrec _ p = shows (positionNumber p) . showsSgnInt 1 (positionTranspose p)

newPosition :: Position
newPosition = APosition 0 0

data FreqTableType = FreqTTypePT | FreqTTypeST | FreqTTypeASM | FreqTTypeRS deriving (Eq, Show, Ord, Enum)


-------------------------------
data Key = KeyC | KeyCS | KeyD  | KeyDS | KeyE | KeyF | KeyFS | KeyG  | KeyGS | KeyA | KeyAS | KeyB deriving (Eq, Show, Ord, Enum)

data Pitch = Pitch
    { pitchKey :: Key
    , pitchOctave :: Int } | Pause | Release | NoNote deriving (Eq)

instance Show Pitch where
    show (Pitch k o) = showChar '<' . showString (["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"] !! fromEnum k) . shows o $ ">"
    show Pause = "<Pse>"
    show Release = "<Rls>"
    show NoNote = "<NoN>"

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
                    , pitchKey = toEnum k }

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
             | EnvFormRepAttackDecay deriving (Eq)

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
                    EnvFormDecay -> 9
                    EnvFormAttack -> 15
                    EnvFormRepDecay -> 8
                    EnvFormRepDecayAttack -> 10
                    EnvFormDecaySus -> 11
                    EnvFormRepAttack -> 12
                    EnvFormAttackSus -> 13
                    EnvFormRepAttackDecay ->14
                    EnvFormNone -> 15

instance Show EnvForm where
    show e = case e of
                  EnvFormDecay          -> "\\__"
                  EnvFormAttack         -> "/|_"
                  EnvFormRepDecay       -> "\\|\\"
                  EnvFormRepDecayAttack -> "\\/\\"
                  EnvFormDecaySus       -> "\\|¯"
                  EnvFormRepAttack      -> "/|/"
                  EnvFormAttackSus      -> "/¯¯"
                  EnvFormRepAttackDecay -> "/\\/"
                  _                     -> "---"

---------------
newtype ChannelMask = ChannelMask { channelMaskValue :: Int } deriving (Eq)

noChannelMask = ChannelMask 7

instance Show ChannelMask where
    showsPrec _ (ChannelMask v) = showsHex 1 v

---------------
type Noise = Maybe Int

toNoise :: (Integral a) => a -> Noise
toNoise = Just . fromIntegral

fromNoise :: (Integral a) => Noise -> a
fromNoise = maybe (-1) fromIntegral

noNoise :: Noise
noNoise = Nothing

---------------
data NoteCmd = NoteCmdNone
             | NoteCmdHldSample
             | NoteCmdHldImage
             | NoteCmdHldInstr
             | NoteCmdDelay Int
             | NoteCmdGlisUp Int Int
             | NoteCmdGlisDn Int Int
             | NoteCmdGlis Int
             | NoteCmdPorta Int Int Int
             | NoteCmdPortaR Int
             | NoteCmdNoise Int
             | NoteCmdSampleOffset Int
             | NoteCmdOrnamentOffset Int
             | NoteCmdVibrato Int Int
             | NoteCmdEnvSlide Int Int
             | NoteCmdVolSlide Int
               deriving (Eq)

instance Show NoteCmd where
    showsPrec _ x = case x of
                  NoteCmdHldSample -> showString "HSmp"
                  NoteCmdHldImage -> showString "HImg"
                  NoteCmdHldInstr -> showString "HIns"
                  NoteCmdDelay d -> showString "D=" . shows2 d
                  NoteCmdGlisUp _ g -> showString "G+" .shows2 g
                  NoteCmdGlisDn _ g -> showString "G-" .shows2 g
                  NoteCmdGlis g -> showString "G=" .shows2 g
                  NoteCmdPorta _ _ p -> showString "P=" .shows2 p
                  NoteCmdPortaR p -> showString "R=" .shows2 p
                  NoteCmdNoise n -> showString "N=" .shows2 n
                  NoteCmdSampleOffset o -> showString "S=" .shows2 o
                  NoteCmdOrnamentOffset o -> showString "O=" .shows2 o
                  NoteCmdVibrato on off -> showString "V=" .shows on .(':':) .shows off
                  NoteCmdEnvSlide d e -> showString "E=" .shows d .(':':) .shows2 e
                  NoteCmdVolSlide s -> showString "A" . showsSgnInt 2 s
                  _ -> showString "----"

data Shared = AShared
    { sharedEnvFreq :: EnvFreq
    , sharedEnvForm :: EnvForm
    , sharedMask :: ChannelMask
    , sharedNoise :: Noise } deriving (Eq)

newShared :: Shared
newShared = AShared noEnvFreq noEnvForm noChannelMask noNoise

instance Show Shared where
    showsPrec _ s = ('{':) . shows (sharedEnvForm s). showsHex 4 (sharedEnvFreq s) .(' ':)
                           . shows (sharedMask s) .(' ':)
                           . maybe ("--" ++ ) (showsHex 2) (sharedNoise s) . ('}':)

data Changed = ChangedSample
             | ChangedOrnament
             | ChangedVolume
             | ChangedEnvEnable
             | ChangedEnvForm
             | ChangedEnvFreq
             | ChangedNoise deriving (Eq, Enum, Bounded, Show)

data Note = ANote
    { noteCmd :: NoteCmd
    , notePitch :: Pitch
    , noteSample :: Int
    , noteOrnament :: Int
    , noteVolume :: Int
    , noteEnvEnable:: Bool
    , noteEnvForm :: EnvForm
    , noteEnvFreq :: EnvFreq
    , noteNoise :: Noise
    , noteFlags :: FlagSet Changed } deriving (Eq)

newNote = ANote NoteCmdNone NoNote 0 0 0 False noEnvForm noEnvFreq noNoise noFlags

instance Show Note where
    showsPrec _ n = ('{':) . shows (notePitch n) . (' ':)
                           . shows (noteSample n) . ('-':)
                           . shows (noteOrnament n) . ('-':)
                           . shows (noteVolume n) . (' ':)
                           . shows (noteEnvForm n). shows (noteEnvFreq n) .(' ':)
                           . shows (noteNoise n) . (' ':)
                           . shows (noteCmd n) . ('}':)

type Channel = [Note]


data Row = ARow
    { rowNumber :: Int
    , rowShared :: Shared
    , rowNotes :: [Note] } deriving (Eq)

makeRowsWithShared :: ([Note] -> Shared) ->  [Channel] -> [Row]
makeRowsWithShared f cs = map makeRow $ zip3 [0..] ss $ tcs
    where
        makeRow (i,s,rs) = ARow i s rs
        tcs = transpose $ cs
        ss = map f tcs

makeRows :: [Channel] -> [Row]
makeRows = makeRowsWithShared (const newShared)

channelsFromRows :: [Row] -> [Channel]
channelsFromRows rs = transpose [ rowNotes r | r <- rs ]

packChannel :: Channel -> [(Note, Maybe Int)]
packChannel ch = snd $ mapAccumL replaceSameWithNoting (newNote, -1) notesCountedZeros
    where
        notesCountedZeros = [(head x, length x - 1) | x <- groupBy (\_ x -> x == newNote) ch]
        replaceSameWithNoting old new  = (new, doReplace old new)
        doReplace (n, a) (x,y)  = (x, if (y == a) then Nothing else Just y)


instance Show Row where
    showsPrec _ r = ('(':) . shows (rowNumber r) . (':':) .shows (rowShared r) . ('|':). showList (rowNotes r) . (')':)

data Pattern = APattern
    { patternNumber :: Int
    , patternRows :: [Row] } deriving (Eq)

newPattern :: Pattern
newPattern = APattern 0 []

instance Show Pattern where
    showsPrec _ pat = showString "Pattern " . shows (patternNumber pat)

-------------------------------
data Instrument d = AnInstrument
    { instrumentNumber :: Int
    , instrumentData :: [d]
    , instrumentLoopStart :: Int
    , instrumentLoopEnd :: Int } deriving (Eq)

newInstrument :: Instrument d
newInstrument = AnInstrument 0 [] 0 0

isEmptyInstrument :: Instrument d -> Bool
isEmptyInstrument (AnInstrument _ [] 0 0) = True
isEmptyInstrument _ = False

---------------

type Sample = Instrument SampleData

pattern Sample {sampleNumber, sampleData, sampleLoopStart, sampleLoopEnd } = AnInstrument sampleNumber sampleData sampleLoopStart sampleLoopEnd
newSample = newInstrument

instance Show Sample where
    showsPrec _ s = ("Sample "++) . shows (sampleNumber s)
        .('(':) . shows (sampleLoopStart s) . ('/':) . shows (sampleLoopEnd s)
        .(")="++) . shows (sampleData s)
---------------

data SampleData = ASampleData
    { sampleDataNoise :: Int
    , sampleDataTone :: Int
    , sampleDataVolume :: Int
    , sampleDataNoiseEnable :: Bool
    , sampleDataNoiseHold :: Bool
    , sampleDataToneEnable :: Bool
    , sampleDataToneHold :: Bool
    , sampleDataEnvEnable :: Bool
    , sampleDataEffect :: SampleDataEffect } deriving (Eq)

newSampleData = ASampleData 0 0 0 False False False False False SDENone

instance Show SampleData where
    showsPrec _ d = ('(':) . shows (sampleDataEffect d) . shows2 (sampleDataVolume d)
                   .((if sampleDataNoiseEnable d then " N=" else " n=") ++) . shows2 (sampleDataNoise d)
                   .((if sampleDataToneEnable d then " T=" else " t=") ++) . showsSgnInt 5 (sampleDataTone d) . (')':)
    showList ds = ('[':) . (intercalate ", " ([shows i . ('-':) $ show d | (i,d) <- zip [0..] ds ]) ++) . (']':)

data SampleDataEffect = SDENone | SDEEnv | SDEDown | SDEUp  deriving (Eq)

instance Show SampleDataEffect where
    show x = case x of
                  SDEEnv  -> "E"
                  SDEUp   -> "+"
                  SDEDown -> "-"
                  _       -> "_"



---------------
type Ornament = Instrument OrnamentData

instance Show Ornament where
    showsPrec _ s = ("Ornament "++) . shows (ornamentNumber s)
                    .('(':) . shows (ornamentLoopStart s) . ('/':) . shows (ornamentLoopEnd s)
                    .(")="++) . shows (ornamentData s)

pattern Ornament {ornamentNumber, ornamentData, ornamentLoopStart, ornamentLoopEnd } = AnInstrument ornamentNumber ornamentData ornamentLoopStart ornamentLoopEnd

newOrnament = newInstrument

---------------
data OrnamentData = AnOrnamentData
    { ornamentDataNoise :: Int
    , ornamentDataTone :: Int
    } deriving (Eq)

instance Show OrnamentData where
    showsPrec _ d = ('(':) . shows (ornamentDataTone d) . ('/':) . shows (ornamentDataNoise d) . (')':)

newOrnamentData :: OrnamentData
newOrnamentData = AnOrnamentData 0 0

