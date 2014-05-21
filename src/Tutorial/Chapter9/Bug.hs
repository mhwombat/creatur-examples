{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}
module Tutorial.Chapter9.Bug (Bug(..), Sex(..), BugColour(..),
  buildBug, run) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Genetics.BRGCBool (Genetic, Sequence,
  DiploidSequence, DiploidReader, getAndExpress,
  runDiploidReader, copy2, consumed2)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists, 
  randomCrossover, randomCutAndSplice, randomOneOfPair, 
  repeatWithProbability, withProbability)
import ALife.Creatur.Genetics.Reproduction.Sexual (Reproductive, Strand, 
  produceGamete, build, makeOffspring)
import ALife.Creatur.Universe (SimpleUniverse, genName, writeToLog)
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
    bugSpots :: [BugColour],
    bugSex :: Sex,
    bugEnergy :: Int,
    bugGenome :: DiploidSequence
  } deriving (Show, Generic)

instance Serialize Bug

instance Agent Bug where
  agentId = bugName
  isAlive bug = bugEnergy bug > 0

instance Record Bug where key = agentId

data BugColour = Green | Purple | Red | Brown | Orange | Pink | Blue
  deriving (Show, Eq, Enum, Bounded, Generic)
instance Serialize BugColour
instance Genetic BugColour
instance Diploid BugColour

data Sex = Male | Female
  deriving (Show, Eq, Enum, Bounded, Generic)
instance Serialize Sex
instance Genetic Sex
instance Diploid Sex

buildBug :: Bool -> String -> DiploidReader (Either [String] Bug)
buildBug truncateGenome name = do
  sex <- getAndExpress
  colour <- getAndExpress
  spots <- getAndExpress
  g <- if truncateGenome then consumed2 else copy2
  return $ Bug name <$> sex <*> colour <*> spots <*> pure 10 <*> pure g

instance Reproductive Bug where
  type Strand Bug = Sequence
  produceGamete a = 
    repeatWithProbability 0.1 randomCrossover (bugGenome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build name = runDiploidReader (buildBug False name)

run :: [Bug] -> StateT (SimpleUniverse Bug) IO [Bug]
run (me:other:_) = do
  writeToLog $ agentId me ++ "'s turn" 
  if bugSex me == Female && bugSex other == Male
    then do
      name <- genName
      (Right baby) <- liftIO $ evalRandIO (makeOffspring me other name)
      writeToLog $ 
        bugName me ++ " and " ++ bugName other ++
          " gave birth to " ++ name ++ ", a " ++ 
          show (bugColour baby) ++ " " ++ show (bugSex baby) ++ " bug"
      writeToLog $ "Mother: " ++ show me
      writeToLog $ "Father: " ++ show other
      writeToLog $ "Baby: " ++ show baby
      return [deductMatingEnergy me, deductMatingEnergy other, baby]
    else return []
run _ = return [] -- need two agents to mate

deductMatingEnergy :: Bug -> Bug
deductMatingEnergy bug = bug {bugEnergy=bugEnergy bug - 1}

