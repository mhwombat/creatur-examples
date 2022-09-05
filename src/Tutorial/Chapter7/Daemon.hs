module Main where

import ALife.Creatur.Daemon    (CreaturDaemon (..), Job (..), launch,
                                simpleDaemon)
import ALife.Creatur.Task      (doNothing, runInteractingAgents, simpleJob)
import ALife.Creatur.Universe  (mkSimpleUniverse)
import System.Directory        (canonicalizePath)
import Tutorial.Chapter7.Plant (run)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter7" -- Required for daemon
  let u = mkSimpleUniverse "Chapter7" dir
  let j = simpleJob
        { task=runInteractingAgents run doNothing doNothing,
          sleepTime=0 }
  launch $ CreaturDaemon (simpleDaemon j u) j
