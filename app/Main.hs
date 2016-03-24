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
        dts  = map ((`H.DateTime` tod) . (H.Date 2016 H.January)) ns
    let ts = S.create dts ns
    print $ ts
