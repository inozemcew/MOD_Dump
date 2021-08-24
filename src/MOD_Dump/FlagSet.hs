module MOD_Dump.FlagSet
    ( FlagSet, noFlags, singleton, toList
    , set, reset, change
    , isSet, isReset, allSet, anySet ) where

import qualified Data.IntSet as S

newtype FlagSet a = AFlagSet { asIntSet :: S.IntSet } deriving Eq

noFlags :: (Enum a) => FlagSet a
noFlags = AFlagSet S.empty

singleton :: (Enum a) => a -> FlagSet a
singleton x = AFlagSet $ S.singleton $ fromEnum x

fromList :: (Enum a) => [a] -> FlagSet a
fromList xs = AFlagSet $ S.fromList $ map fromEnum xs

toList :: (Enum a) => FlagSet a -> [a]
toList x = toEnum <$> S.toList (asIntSet x)

set :: (Enum a) => FlagSet a -> a -> FlagSet a
set s x = s { asIntSet = S.insert (fromEnum x) (asIntSet s) }

reset :: (Enum a) => FlagSet a -> a -> FlagSet a
reset s x = s { asIntSet = S.delete (fromEnum x) (asIntSet s) }

change :: (Enum a) => FlagSet a -> a -> Bool -> FlagSet a
change s x b = if b then set s x else reset s x

isSet :: (Enum a) => FlagSet a -> a -> Bool
isSet s a = fromEnum a `S.member` asIntSet s

isReset :: (Enum a) => FlagSet a -> a -> Bool
isReset s x = not $ isSet s x

allSet :: (Enum a) => FlagSet a -> [a] -> Bool
allSet s as = all (`S.member` asIntSet s) $ map fromEnum as

anySet :: (Enum a) => FlagSet a -> [a] -> Bool
anySet s as = any (`S.member` asIntSet s) $ map fromEnum as

allReset :: (Enum a) => FlagSet a -> [a] -> Bool
allReset s as = not $ any (`S.member` asIntSet s) $ map fromEnum as


instance (Enum a, Show a) => Show (FlagSet a)
    where
        showsPrec _ x = showList (toList x)
