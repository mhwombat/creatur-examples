module Main where

import Tutorial.Example3.Bug (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Universe.Task (simpleDaemon, runInteractingAgents)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "example3" -- Required for daemon
  let u = mkSimpleUniverse "Example3" dir 100000
  launch simpleDaemon{task=runInteractingAgents run} u

