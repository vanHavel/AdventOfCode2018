module Main where

import System.Environment

import Switcher

main :: IO ()
main = do
  day <- read <$> head <$> getArgs
  Switcher.runDay day
