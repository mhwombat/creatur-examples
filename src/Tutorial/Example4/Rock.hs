{-# LANGUAGE DeriveGeneric #-}

module Tutorial.Example4.Rock (Rock(..), run) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Universe (SimpleUniverse)
import ALife.Creatur.Logger (writeToLog)
import Control.Monad.State ( StateT )
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Rock = Rock String Int deriving (Show, Generic)

instance Serialize Rock

instance Agent Rock where
  agentId (Rock name _) = name
  isAlive _ = True

instance Record Rock where key = agentId

