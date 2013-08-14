{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}
module Tutorial.Chapter10.Bug (Bug(..), Sex(..), BugColour(..), 
  buildBug, tryMating) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.AgentNamer (genName)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Genetics.BRGCWord8 (Genetic, Sequence,
  DiploidSequence, DiploidReader, getAndExpress, runDiploidReader,
  copy2)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists, 
  randomCrossover, randomCutAndSplice, randomOneOfPair, 
  repeatWithProbability, withProbability)
import ALife.Creatur.Genetics.Reproduction.Sexual (Reproductive, Base, 
  produceGamete, build, makeOffspring)
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (StateT)
import Data.Serialize (Serialize)

import GHC.Generics (Generic)

data Bug = Bug
  { 
    bugName :: String,
    bugColour :: BugColour,
    bugSex :: Sex,
    bugEnergy :: Int,
    bugGenome :: DiploidSequence
  } deriving (Show, Generic)

instance Serialize Bug

instance Agent Bug where
  agentId = bugName
  isAlive bug = bugEnergy bug > 0

instance Record Bug where key = agentId

data BugColour = Green | Purple
  deriving (Show, Eq, Enum, Bounded, Generic)
instance Serialize BugColour
instance Genetic BugColour
instance Diploid BugColour

data Sex = Male | Female
  deriving (Show, Eq, Enum, Bounded, Generic)
instance Serialize Sex
instance Genetic Sex
instance Diploid Sex

buildBug :: String -> DiploidReader (Maybe Bug)
buildBug name = do
  g <- copy2
  sex <- getAndExpress
  colour <- getAndExpress
  return $ Bug name <$> colour <*> sex <*> pure 10 <*> pure g

instance Reproductive Bug where
  type Base Bug = Sequence
  produceGamete a = 
    repeatWithProbability 0.1 randomCrossover (bugGenome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build name = runDiploidReader (buildBug name)

tryMating :: [Bug] -> StateT (SimpleUniverse a) IO [Bug]
tryMating (me:other:_) = do
  writeToLog $ bugName me ++ ", a " ++ show (bugSex me) ++ " bug, sees "
    ++ bugName other ++ ", a " ++ show (bugSex other)
  if bugSex me == Female && bugSex other == Male
    then do
      name <- genName
      (Just baby) <- liftIO $ evalRandIO (makeOffspring me other name)
      writeToLog $ 
        bugName me ++ " and " ++ bugName other ++
          " gave birth to " ++ name ++ ", a " ++ 
          show (bugColour baby) ++ " " ++ show (bugSex baby) ++ " bug"
      writeToLog $ "Mother: " ++ show me
      writeToLog $ "Father: " ++ show other
      writeToLog $ "Baby: " ++ show baby
      return [deductMatingEnergy me, deductMatingEnergy other, baby]
    else do
      writeToLog $ bugName me ++ " is not interested in "
        ++ bugName other
      return [me, other]
tryMating x = return x -- need two agents to mate

deductMatingEnergy :: Bug -> Bug
deductMatingEnergy bug = bug {bugEnergy=bugEnergy bug - 1}

