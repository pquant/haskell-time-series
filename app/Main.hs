{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- import Lib
import qualified Data.Hourglass as H
import qualified Data.Series as S

main :: IO ()
-- main=someFunc
main = do
----------------------------------------------------------
-- Play with timeDiffs on DateTimes
----------------------------------------------------------
    let dt1 = H.DateTime (H.Date 2016 H.January 1) (H.TimeOfDay 19 40 1 0)
        dt2 = H.DateTime (H.Date 2016 H.January 2) (H.TimeOfDay 19 40 0 0)
        diff = H.timeDiff dt2 dt1
        diffP = H.timeDiffP dt2 dt1
    print "diff:" >> print diff
    print "diffP:" >> print diffP
    print "-----------------------------------"
----------------------------------------------------------
-- Play with time-series creation
----------------------------------------------------------
    let ns = [1 ..10]
        tod = H.TimeOfDay 19 40 0 0
        ds  = map (H.Date 2016 H.January) ns
        dts = map (`H.DateTime` tod) ds
        ldts  = map H.localTimeFromGlobal dts
        ts0 = S.create ds ns
        ts1 = S.create dts ns
        ts2 = S.create ldts ns
    print ts0
    print ts1
    print ts2
    print "-----------------------------------"
----------------------------------------------------------
-- More Play with timeDiffs
----------------------------------------------------------
    -- Time difference tests
    let todPlus2Secs = H.DateTime (H.Date 2016 H.January 1) $ tod {H.todSec = H.Seconds 2}
        todPlus1Sec = H.DateTime (H.Date 2016 H.January 1) $ tod {H.todSec = H.Seconds 1}
        todPlus1NanoSec = H.DateTime (H.Date 2016 H.January 1) $ tod {H.todNSec = H.NanoSeconds 1}
        todPlusLoadsNanoSecs = H.DateTime (H.Date 2016 H.January 1) $ tod {H.todNSec = H.NanoSeconds 10^9+1}
        todRef = H.DateTime (H.Date 2016 H.January 1) $ tod
    print (H.timeDiff todPlus2Secs todRef)
    print (H.timeDiff todPlus1Sec todRef)
    print (H.timeDiff todPlus1NanoSec  todRef)
    print (H.timeDiff todPlusLoadsNanoSecs todRef)

    print (H.timeDiffP todPlus2Secs todRef)
    print (H.timeDiffP todPlus1Sec todRef)
    print (H.timeDiffP todPlus1NanoSec  todRef)
    print (H.timeDiffP todPlusLoadsNanoSecs todRef)


    print ((\(x,y) -> x + H.toSeconds y)$ H.timeDiffP todPlus2Secs todRef)
    print ((\(x,y) -> x + H.toSeconds y)$ H.timeDiffP todPlus1Sec todRef)
    print ((\(x,y) -> x + H.toSeconds y)$ H.timeDiffP todPlus1NanoSec  todRef)
    print ((\(x,y) -> x + H.toSeconds y)$ H.timeDiffP todPlusLoadsNanoSecs todRef)
    print "-----------------------------------"
