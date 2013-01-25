{-# LANGUAGE DeriveGeneric #-}
module Tutorial.Example3.Agent (Agent, run) where

import Tutorial.Example3.Rock (Rock, runRock)
import Tutorial.Example3.Bug (Bug, runBug)
import ALife.Creatur (Agent, AgentId, agentId, isAlive)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Util (stateMap)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Agent = RockBox Rock | BugBox Bug deriving (Show, Generic)

instance Serialize Agent

instance Record Agent where
  key (RockBox r) = "Rock_" ++ agentId r
  key (BugBox b) = "Bug_" ++ agentId b

runBox :: Agent -> StateT (SimpleUniverse Agent) IO Agent
runBox (RockBox (Rock name k)) = do
  -- Write to the log file so we can verify that the rock gets a 
  -- chance to use the CPU regularly.
  writeToLog $ "\"" ++ name ++ 
    "\" the rock is thinking about the meaning of life. Counter=" ++ show k
  -- Return a new version of this rock, with an updated counter.
  return $ Rock name (k+1)
runBox (BugBox bug) = do
  writeToLog $ 
    describe bug ++ " just woke up. Energy=" ++ show (bugEnergy bug)
  bug2 <- maybeMate bug
  let bug3 = bug2{ bugEnergy = bugEnergy bug2 - 1}
  if isAlive bug3
    then
      writeToLog $ 
        describe bug3 ++ " is going back to sleep. Energy=" 
          ++ show (bugEnergy bug3)
    else writeToLog $ bugName bug ++ " is dead"
  return bug3

--type AgentProgram c l d x a = a → StateT (Universe c l d x a) IO a

--stateMap ∷ Monad m ⇒ (s → t) → (t → s) → StateT s m a → StateT t m a

--NEED TO RETHINK THIS. NEED TO LIFT:
--runRock :: Rock -> StateT (SimpleUniverse Rock) IO Rock
--TO:
--runBox :: Agent -> StateT (SimpleUniverse Agent) IO Agent

--THIS WON'T WORK:
--run (RockBox r) = stateMap wrapRock unwrapRock $ runRock r
--run (BugBox b) = stateMap wrapBug unwrapBug $ runBug b

