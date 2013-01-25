module Main where

import Tutorial.Example2.Plant (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Universe.Task (simpleDaemon, runInteractingAgents)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "example2" -- Required for daemon
  let u = mkSimpleUniverse "Example2" dir 100000
  launch simpleDaemon{task=runInteractingAgents run} u

