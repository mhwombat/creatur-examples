module Main where

import Tutorial.Chapter8.Bug (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Universe.Task (simpleDaemon, runInteractingAgents)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter8" -- Required for daemon
  let u = mkSimpleUniverse "Chapter8" dir 100000
  launch simpleDaemon{task=runInteractingAgents run} u

