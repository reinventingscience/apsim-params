module Main (main) where

import APSIM.Client

main :: IO ()
main = runAPSIMClient defaultAPSIMServer defaultAPSIMServerPort
