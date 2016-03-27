module Data.Series
(
-- * Type
Series, -- export Type only. The official constructor is `create`

-- * Construction/Conversion
create, createUnsafe,
toTuples

) where

import qualified Data.Hourglass as H
import qualified Data.Map       as M
import qualified Data.List      as L

--------------------------------------------------------------
-- SeriesTime class
--------------------------------------------------------------
-- | SeriesTime class to identify which Hourglass time type is suitable for time-series
--
-- (Not all of them are, plus some functionalities are time-type specific)
class (Ord t, Eq t) => SeriesTime t where
    printSeriesTime :: H.TimeFormat frmt => frmt -> t -> String

instance (H.Time t, Ord t, Eq t) => SeriesTime (H.LocalTime t) where
    printSeriesTime = H.localTimePrint

instance SeriesTime H.DateTime where
    printSeriesTime = H.timePrint

--------------------------------------------------------------
-- Series Type
--------------------------------------------------------------
-- | Series data type
data Series t a = Series (M.Map t a) deriving(Eq)

instance (SeriesTime t, Show a) => Show (Series t a) where
    show ts =
        let
            frmt = H.ISO8601_DateAndTime
            f :: (SeriesTime t, Show a) => (t, a) -> String
            f (ldt,x) = (printSeriesTime frmt ldt) ++ "," ++ (show x)
        in
            unlines $ L.map f $ toTuples ts

--------------------------------------------------------------
-- Construction/Conversion
--------------------------------------------------------------
create
    :: SeriesTime t
    => [t] -- Times (:[LocalDateTime])
    -> [a]          -- Vals (:[a])
    -> Series t a
create xs ys = Series $ M.fromList $ zip xs ys

createUnsafe
    :: SeriesTime t
    => [t] -- ^ Times (:[LocalDateTime])
    -> [a]          -- ^ Vals (:[a])
    -> Series t a
createUnsafe xs ys = Series $ M.fromAscList $ zip xs ys

-- | Returns a list of 2-tuples from a Series a
toTuples
    :: SeriesTime t
    => Series t a -- ^ Series (:Series a)
    -> [(t, a)]
toTuples (Series m) = M.toAscList m
