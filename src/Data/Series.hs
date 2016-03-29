module Data.Series
(
-- * Type
Series, -- export Type only. The official constructor is `create`

-- * Construction/Conversion
create, createUnsafe,
toTuples,

-- * Accessors
values
) where

import qualified Data.Hourglass as H
import qualified Data.Map       as M
import qualified Data.List      as L

--------------------------------------------------------------
-- TimeStamp class
--------------------------------------------------------------
-- | TimeStamp class to identify which Hourglass time type is suitable for time-series
--
-- (Not all of them are, plus some functionalities are time-type specific)
class (Ord t, Eq t) => TimeStamp t where
    printTimeStamp :: H.TimeFormat frmt => frmt -> t -> String

instance (H.Time t, Ord t, Eq t) => TimeStamp (H.LocalTime t) where
    printTimeStamp = H.localTimePrint

instance TimeStamp H.DateTime where
    printTimeStamp = H.timePrint

--------------------------------------------------------------
-- Series Type
--------------------------------------------------------------
-- | Series data type
data Series t a = Series (M.Map t a) deriving(Eq)

instance (TimeStamp t, Show a) => Show (Series t a) where
    show ts =
        let
            frmt = H.ISO8601_DateAndTime
            f :: (TimeStamp t, Show a) => (t, a) -> String
            f (ldt,x) = (printTimeStamp frmt ldt) ++ "," ++ (show x)
        in
            unlines $ L.map f $ toTuples ts

--------------------------------------------------------------
-- Construction/Conversion
--------------------------------------------------------------
create
    :: TimeStamp t -- (Ord t, Eq t)
    => [t]
    -> [a]
    -> Series t a
create xs ys = Series $ M.fromList $ zip xs ys

createUnsafe
    :: TimeStamp t
    => [t]
    -> [a]
    -> Series t a
createUnsafe xs ys = Series $ M.fromAscList $ zip xs ys

-- | Returns a list of 2-tuples from a Series a
toTuples
    :: TimeStamp t
    => Series t a
    -> [(t, a)]
toTuples (Series m) = M.toAscList m

--------------------------------------------------------------
-- Accessors
--------------------------------------------------------------
-- | Returns the values of Series a
values
    :: Series t a
    -> [a]
values (Series m)= M.elems m
