module Main where

import ALife.Creatur.Daemon   (CreaturDaemon (..), Job (..), launch,
                               simpleDaemon)
import ALife.Creatur.Task     (doNothing, runInteractingAgents, simpleJob)
import ALife.Creatur.Universe (mkSimpleUniverse)
import System.Directory       (canonicalizePath)
import Tutorial.Chapter8.Bug  (run)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter8" -- Required for daemon
  let u = mkSimpleUniverse "Chapter8" dir
  let j = simpleJob
        { task=runInteractingAgents run doNothing doNothing,
          sleepTime=0 }
  launch $ CreaturDaemon (simpleDaemon j u) j

