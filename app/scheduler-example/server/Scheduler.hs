module Main where

import App.Scheduler
import Beckn.Prelude

main :: IO ()
main = runExampleScheduler identity
