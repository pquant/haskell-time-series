{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Main where

-- import Lib
import qualified Data.Hourglass as H
import qualified Data.Series as S

main :: IO ()
-- main=someFunc
main = do
    let ns = [1 ..10]
        tod = H.TimeOfDay 19 40 0 0
        ds  = map (H.Date 2016 H.January) ns
        dts = map (`H.DateTime` tod) ds
        ldts  = map H.localTimeFromGlobal dts
        --ts0 = S.create ds ns
        ts1 = S.create dts ns
        ts2 = S.create ldts ns
    --print $ S.values ts0
    print ts1
    print ts2
