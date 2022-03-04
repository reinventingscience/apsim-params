{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import APSIM.Client
import APSIM.Data

myAPSIMSession :: APSIMSession
myAPSIMSession = APSIMSession "Report"
  [ Variable "Clock.Today" (Some APSIMDate)
  , Variable "Zone" (Some APSIMString)
  , Variable "Soil.SoilWater.SW(1)" (Some APSIMDouble)
  , Variable "Yield" (Some APSIMDouble)
  ]

main :: IO ()
main = runAPSIMClient defaultAPSIMServer defaultAPSIMServerPort myAPSIMSession
