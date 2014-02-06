module Main where

import Tutorial.Chapter10.Martian (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Task (simpleDaemon, runInteractingAgents,
  noSummary)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter10" -- Required for daemon
  let u = mkSimpleUniverse "Chapter10" dir 100000
  launch simpleDaemon{task=runInteractingAgents run noSummary} u

