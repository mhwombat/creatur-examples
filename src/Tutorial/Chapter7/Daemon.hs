module Main where

import Tutorial.Chapter7.Plant (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Task (simpleDaemon, runInteractingAgents,
  noSummary)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter7" -- Required for daemon
  let u = mkSimpleUniverse "Chapter7" dir 100000
  launch simpleDaemon{task=runInteractingAgents run noSummary} u

