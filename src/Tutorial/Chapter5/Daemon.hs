module Main where

import ALife.Creatur.Daemon   (CreaturDaemon (..), Job (..), launch,
                               simpleDaemon)
import ALife.Creatur.Task     (doNothing, runNoninteractingAgents, simpleJob)
import ALife.Creatur.Universe (mkSimpleUniverse)
import System.Directory       (canonicalizePath)
import Tutorial.Chapter5.Rock (run)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter5" -- Required for daemon
  let u = mkSimpleUniverse "Chapter5" dir
  let j = simpleJob
        { task=runNoninteractingAgents run doNothing doNothing,
          sleepTime=0 }
  launch $ CreaturDaemon (simpleDaemon j u) j
