{-# LANGUAGE DeriveGeneric #-}

module Tutorial.Chapter10.Rock (Rock(..)) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.Database (Record, key)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Rock = Rock String Int deriving (Show, Generic)

instance Serialize Rock

instance Agent Rock where
  agentId (Rock name _) = name
  isAlive _ = True

instance Record Rock where key = agentId

