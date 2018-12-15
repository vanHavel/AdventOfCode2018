module Switcher(runDay) where

import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Day7
import Days.Day8
import Days.Day9
import Days.Day10
import Days.Day11

runDay :: Int -> String -> String
runDay 1 = Days.Day1.run
runDay 2 = Days.Day2.run 
runDay 3 = Days.Day3.run 
runDay 4 = Days.Day4.run
runDay 5 = Days.Day5.run
runDay 6 = Days.Day6.run
runDay 7 = Days.Day7.run
runDay 8 = Days.Day8.run
runDay 9 = Days.Day9.run
runDay 10 = Days.Day10.run
runDay 11 = Days.Day11.run