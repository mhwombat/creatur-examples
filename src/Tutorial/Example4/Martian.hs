{-# LANGUAGE DeriveGeneric #-}
module Tutorial.Example4.Martian (Martian(..), run) where

import Tutorial.Example4.Rock (Rock)
import Tutorial.Example4.Plant (Plant)
import qualified Tutorial.Example4.Plant as P (tryMating)
import Tutorial.Example4.Bug (Bug)
import qualified Tutorial.Example4.Bug as B (tryMating)
import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse)
import Control.Monad.State (StateT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Martian = FromRock Rock | FromPlant Plant | FromBug Bug
  deriving (Show, Generic)

instance Serialize Martian

instance Agent Martian where
  agentId (FromRock x) = agentId x
  agentId (FromPlant x) = agentId x
  agentId (FromBug x) = agentId x
  isAlive (FromRock x) = isAlive x
  isAlive (FromPlant x) = isAlive x
  isAlive (FromBug x) = isAlive x

instance Record Martian where
  key = agentId

run :: [Martian] -> StateT (SimpleUniverse Martian) IO [Martian]
run xs@(me:_) = do
  writeToLog $ agentId me ++ "'s turn"
  tryMating xs
run [] = error "empty agent list"

tryMating :: [Martian] -> StateT (SimpleUniverse Martian) IO [Martian]
tryMating (FromPlant me:FromPlant other:_) = do
    xs <- P.tryMating [me, other]
    return $ map FromPlant xs
tryMating (FromBug me:FromBug other:_) = do
    xs <- B.tryMating [me, other]
    return $ map FromBug xs
tryMating xs = return xs -- can't mate rocks or mismatched species

