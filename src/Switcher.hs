module Switcher(runDay) where

import Days.Day1
import Days.Day2

runDay :: Int -> String -> String
runDay 1 = Days.Day1.run
runDay 2 = Days.Day2.run 
