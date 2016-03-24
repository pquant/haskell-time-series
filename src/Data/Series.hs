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
-- | Alias for LocalTime t::DateTime
type LocalDateTime = H.LocalTime H.DateTime

-- | Series data type
data Series a = Series (M.Map LocalDateTime a) deriving(Eq)
instance Show a => Show (Series a) where
    show ts =
      let
        f :: LocalDateTime -> String
        f = (H.timePrint H.ISO8601_DateAndTime) . (H.localTimeUnwrap::LocalDateTime->H.DateTime)
        g :: Show a => (LocalDateTime, a) -> String
        g (ldt,x) = (f ldt) ++ "," ++ (show x)
      in
        unlines $ L.map g $ toTuples ts

--------------------------------------------------------------
-- Construction/Conversion
--------------------------------------------------------------
-- | Series construction
create
    :: [LocalDateTime] -- Times (:[LocalDateTime])
    -> [a]          -- Vals (:[a])
    -> Series a
create xs ys = Series $ M.fromList $ zip xs ys

createUnsafe
    :: [LocalDateTime] -- ^ Times (:[LocalDateTime])
    -> [a]          -- ^ Vals (:[a])
    -> Series a
createUnsafe xs ys = Series $ M.fromAscList $ zip xs ys

-- | Returns a list of 2-tuples from a Series a
toTuples
    :: Series a -- ^ Series (:Series a)
    -> [(LocalDateTime, a)]
toTuples (Series m) = M.toAscList m

-- | Generates a Series a from a list of LocalDateTime and a mapping of these
fromDates
    :: [LocalDateTime]       -- ^ datetimes (:[LocalDateTime])
    -> (LocalDateTime -> a)  -- ^ Map (:(LocalDateTime -> a))
    -> Series a
fromDates xs f = create xs $ L.map f xs
