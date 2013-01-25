module Main where

import Tutorial.Example4.Martian (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Universe.Task (simpleDaemon, runInteractingAgents)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "example4" -- Required for daemon
  let u = mkSimpleUniverse "Example4" dir 100000
  launch simpleDaemon{task=runInteractingAgents run} u

