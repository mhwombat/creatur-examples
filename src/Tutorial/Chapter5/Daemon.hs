module Main where

import Tutorial.Chapter5.Rock (run)
import ALife.Creatur.Daemon (CreaturDaemon(..), Job(..),
  simpleDaemon, launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Task (simpleJob, runNoninteractingAgents,
  doNothing)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter5" -- Required for daemon
  let u = mkSimpleUniverse "Chapter5" dir
  let j = simpleJob
        { task=runNoninteractingAgents run doNothing doNothing,
          sleepTime=0 }
  launch $ CreaturDaemon (simpleDaemon j u) j
