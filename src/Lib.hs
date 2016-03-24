{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Lib
    ( someFunc
    ) where

import qualified  Data.Hourglass as H

someFunc :: IO ()
someFunc = do
    -- Basic Date/TimeOfDay/Datetime construction
    let d = H.Date 2016 H.January 1
        tod =  H.TimeOfDay 19 40 0 0
        dt = H.DateTime d tod
    print d
    print tod
    print $ H.timeGetElapsed dt
    print $ H.timeGetElapsedP dt
    -- Formatted display
    let dash = H.Format_Text '-'
        frmt1 = H.ISO8601_DateAndTime -- H.TimeFormatString [H.Format_Year4, dash, H.Format_MonthName_Short, dash, H.Format_Day2, H.Format_Spaces, H.Format_TimezoneName]
    putStrLn $ H.timePrint frmt1 d
    putStrLn $ H.timePrint frmt1 dt
    -- Local tile
    let tz_offset = H.TimezoneOffset 300
        lt = H.localTime tz_offset dt
        dt_with_offset = H.localTimeToGlobal lt
    print $ H.timePrint frmt1 dt_with_offset
