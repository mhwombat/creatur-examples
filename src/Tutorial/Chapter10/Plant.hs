{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}
module Tutorial.Chapter10.Plant (Plant(..), FlowerColour(..),
  buildPlant, tryMating) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.AgentNamer (genName)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Genetics.BRGCWord8 (Genetic, Reader, Sequence,
  getWithDefault, runReader, copy)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists, 
  randomCrossover, randomCutAndSplice, randomOneOfPair, withProbability)
import ALife.Creatur.Genetics.Reproduction.Asexual (Reproductive, Base, 
  recombine, build, makeOffspring)
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (StateT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Plant = Plant
  { 
    plantName :: String,
    plantFlowerColour :: FlowerColour,
    plantEnergy :: Int,
    plantGenome :: Sequence
  } deriving (Show, Generic)

instance Serialize Plant

instance Agent Plant where
  agentId = plantName
  isAlive plant = plantEnergy plant > 0

instance Record Plant where key = agentId

data FlowerColour = Red | Orange | Yellow | Violet | Blue
  deriving (Show, Eq, Generic, Enum, Bounded)
instance Serialize FlowerColour
instance Genetic FlowerColour

buildPlant :: String -> Reader (Maybe Plant)
buildPlant name = do
  g <- copy
  colour <- getWithDefault Red
  return . Just $ Plant name colour 10 g

instance Reproductive Plant where
  type Base Plant = Sequence
  recombine a b = 
    withProbability 0.1 randomCrossover (plantGenome a, plantGenome b) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build name = runReader (buildPlant name)


tryMating :: [Plant] -> StateT (SimpleUniverse a) IO [Plant]
tryMating (me:other:_) = do
  name <- genName
  (Just baby) <- liftIO $ evalRandIO (makeOffspring me other name)
  writeToLog $ 
    plantName me ++ " and " ++ plantName other ++
      " gave birth to " ++ name ++ ", with " ++ 
       show (plantFlowerColour baby) ++ " flowers"
  writeToLog $ "Me: " ++ show me
  writeToLog $ "Mate: " ++ show other
  writeToLog $ "Baby: " ++ show baby
  return [deductMatingEnergy me, deductMatingEnergy other, baby]
tryMating x = return x -- need two agents to mate

deductMatingEnergy :: Plant -> Plant
deductMatingEnergy p = p {plantEnergy=plantEnergy p - 1}

