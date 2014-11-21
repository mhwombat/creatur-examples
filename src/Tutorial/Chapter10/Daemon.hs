module Main where

import Tutorial.Chapter10.Martian (run)
import ALife.Creatur.Daemon (CreaturDaemon(..), Job(..),
  simpleDaemon, launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Task (simpleJob, runInteractingAgents,
  doNothing)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter10" -- Required for daemon
  let u = mkSimpleUniverse "Chapter10" dir
  let j = simpleJob
        { task=runInteractingAgents run doNothing doNothing,
          sleepTime=0 }
  launch $ CreaturDaemon (simpleDaemon j u) j

