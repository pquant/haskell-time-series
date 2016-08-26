{-#LANGUAGE ScopedTypeVariables#-}
module Data.Series
(
-- * Type
Series, -- export Type only. The official (safe) constructor is `create`

-- * Construction/Conversion
create, createUnsafe,
fromTimeStamps,
-- * Conversion
toTuples,

-- * Accessors
values, timeStamps,
lookup, (!)
) where

import Prelude hiding (lookup)
import qualified Data.Hourglass as H
import qualified Data.Map       as M
import qualified Data.List      as L
import Data.Function (on)
--------------------------------------------------------------
-- TimeStamp class
--------------------------------------------------------------
-- | TimeStamp class to identify which Hourglass time type is suitable for time-series
--
-- (Not all of them are, plus some functionalities are time-type specific)
--
-- A TimeStamp has to be a Timeable because HourGlass.timeDiff is called in the safe contructor function 'create'.
-- Also TimeStamp t has to be an instance of Eq as safe construction requires constructing a Map with fromAscList::Eq t => [(t,a)] -> Map t a
--class (H.Timeable t, Eq t)  => TimeStamp t where
class (H.Timeable t, Eq t, Ord t)  => TimeStamp t where
    printTimeStamp :: H.TimeFormat frmt => frmt -> t -> String

-- Extra Time t context required as (Ord t, Time t) => Ord (LocalTime t) - TimeStamp provides Ord t but not Time t
instance (TimeStamp t, H.Time t) => TimeStamp (H.LocalTime t) where
    printTimeStamp = H.localTimePrint

instance TimeStamp H.DateTime where
    printTimeStamp = H.timePrint

instance TimeStamp H.Date where
    printTimeStamp = H.timePrint

--------------------------------------------------------------
-- Extra Timeable instances
--
-- Confirm with Vincent and see if that can be integrated into Hourglass
--------------------------------------------------------------
instance H.Timeable t => H.Timeable (H.LocalTime t) where
    timeGetElapsedP = H.timeGetElapsedP . H.localTimeUnwrap

--------------------------------------------------------------
-- Series Type
--------------------------------------------------------------
-- | Series data type
--
-- Does not yet allow sub-seccond difference between Timestamps if constructed using the safe `create` function
data Series t a = Series (M.Map t a) deriving(Eq)

instance (TimeStamp t, Show a) => Show (Series t a) where
    show ts =
        let
            frmt = H.ISO8601_DateAndTime
            f :: (TimeStamp t, Show a) => (t, a) -> String
            f (ldt,x) = printTimeStamp frmt ldt ++ "," ++ show x
        in
            unlines $ L.map f $ toTuples ts

--------------------------------------------------------------
-- Construction/Conversion
--------------------------------------------------------------
-- | Safe time-series construction which guarantees underlying [(timestamp,a)] to be ordered by timestamp and each timestamp to be unique
--
-- Uses Data.List.sort (mergeSort) on t - should be O(nlogn) best/worst/average
--
-- Further uses Map.fromAscList which is O(n) => Overall : O(nlogn)
--
-- Uniqueness of timestamps in fact requires timestamps not to be less than 1 second apart (TODO: make that nanosecs)
create :: TimeStamp t => [t] -> [a] -> Series t a
create ts xs = Series $ M.fromAscList $ L.sortBy (compareFun `on` fst) $ zip ts xs
    where
        compareFun :: TimeStamp t => t -> t -> Ordering
        compareFun ti tj
            | abs diffSecs < 1  = error errMsg
            | otherwise         = if signum diffSecs == 1 then GT else LT
            where
                diffSecs= secs + H.toSeconds nanoSecs where (secs, nanoSecs) = H.timeDiffP ti tj
                errMsg =    "\nSafe time-series constructions currently requires TimeStamps not to be less than 1 second apart, " ++
                            "otherwise TimeStamps are considered equal, which cannot happen in time-series.\n" ++
                            "The following time-stamps do not satisfy this criterion:\n" ++
                            printTimeStamp H.ISO8601_DateAndTime ti ++ " and " ++ printTimeStamp H.ISO8601_DateAndTime tj ++
                            " have a time difference (in seconds) of " ++ show diffSecs


-- | Assumes TimeStamps are in increasing order and distinguishable (i.e. no less than 1 second apart)
createUnsafe :: TimeStamp t => [t] -> [a] -> Series t a
createUnsafe ts xs = Series $ M.fromAscList $ zip ts xs

-- | Generates time-series from an unordered list of (timestamp, value) tuples
fromTuples :: TimeStamp t => [(t,a)] -> Series t a
fromTuples = uncurry create . unzip

-- | Generates time-series from an unordered list of timestamps
fromTimeStamps :: TimeStamp t => [t] -> (t -> a) -> Series t a
fromTimeStamps ts f = create ts $ L.map f ts

----------------------------------------------------------------
-- Conversion
----------------------------------------------------------------
-- | Returns a list of 2-tuples from a Series a (O(n))
toTuples :: TimeStamp t => Series t a -> [(t, a)]
toTuples (Series m) = M.toList m

----------------------------------------------------------------
---- Accessors
----------------------------------------------------------------
-- | Returns the values of Series a (O(n))
values :: Series t a -> [a]
values (Series m) = snd . unzip . M.toList $ m

-- | Returns the timeStamps of Series a (O(n))
timeStamps :: Series t a -> [t]
timeStamps (Series m) = fst . unzip . M.toList $ m

-- O(logn)
lookup :: TimeStamp t => t -> Series t a -> Maybe a
lookup t (Series m) = M.lookup t m

-- O(logn)
(!) :: TimeStamp t => Series t a -> t -> a
(!) ts t = case t `lookup` ts of
    Just x -> x
    Nothing -> error $ "\nCouldn't find time-series value at timestamp " ++ printTimeStamp H.ISO8601_DateAndTime t
