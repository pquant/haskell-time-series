module Data.Series
(
-- * Type
Series, -- export Type only. The official constructor is `create`

-- * Construction/Conversion
create, createUnsafe,

) where

import qualified Data.Hourglass as H
import qualified Data.Map       as M
import qualified Data.List      as L


--------------------------------------------------------------
-- Type
--------------------------------------------------------------
-- | Series data type
data Series a = Series (M.Map H.DateTime a) deriving(Eq)
instance (Show a) => Show (Series a) where
    show ts = unlines $ L.map (\(dt,x) -> (H.timePrint H.ISO8601_DateAndTime dt) ++ "," ++ (show x)) $ toTuples ts

--------------------------------------------------------------
-- Construction/Conversion
--------------------------------------------------------------
-- | Series construction
create
    :: [H.DateTime] -- Times (:[H.DateTime])
    -> [a]          -- Vals (:[a])
    -> Series a
create xs ys = Series $ M.fromList $ zip xs ys

createUnsafe
    :: [H.DateTime] -- ^ Times (:[H.DateTime])
    -> [a]          -- ^ Vals (:[a])
    -> Series a
createUnsafe xs ys = Series $ M.fromAscList $ zip xs ys

-- | Returns a list of 2-tuples from a Series a
toTuples
    :: Series a -- ^ Series (:Series a)
    -> [(H.DateTime, a)]
toTuples (Series m) = M.toAscList m

-- | Generates a Series a from a list of H.DateTime and a mapping of these
fromDates
    :: [H.DateTime]       -- ^ datetimes (:[H.DateTime])
    -> (H.DateTime -> a)  -- ^ Map (:(H.DateTime -> a))
    -> Series a
fromDates xs f = create xs $ L.map f xs
