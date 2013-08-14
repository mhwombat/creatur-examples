module Main where

import Tutorial.Chapter5.Rock (run)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (mkSimpleUniverse)
import ALife.Creatur.Universe.Task (simpleDaemon, 
  runNoninteractingAgents)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
  dir <- canonicalizePath "chapter5" -- Required for daemon
  let u = mkSimpleUniverse "Chapter5" dir 100000
  launch simpleDaemon{task=runNoninteractingAgents run} u

