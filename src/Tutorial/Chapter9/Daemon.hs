module Main where

import Tutorial.Chapter9.Bug (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Task (simpleDaemon, runInteractingAgents,
  noSummary)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter9" -- Required for daemon
  let u = mkSimpleUniverse "Chapter9" dir
  launch simpleDaemon{task=runInteractingAgents run 2 noSummary} u

