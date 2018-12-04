module Switcher(runDay) where

import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4

runDay :: Int -> String -> String
runDay 1 = Days.Day1.run
runDay 2 = Days.Day2.run 
runDay 3 = Days.Day3.run 
runDay 4 = Days.Day4.run