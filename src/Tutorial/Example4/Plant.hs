{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}
module Tutorial.Example4.Plant (Plant(..), FlowerColour(..), colourCode, 
  buildPlant, tryMating) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.AgentNamer (genName)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Genetics.Code (Code, mkGrayCode, decodeNext)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists, 
  randomCrossover, randomCutAndSplice, randomOneOfPair, withProbability)
import ALife.Creatur.Genetics.Reproduction.Asexual (Reproductive, Base, 
  recombine, build, makeOffspring)
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (StateT)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Plant = Plant
  { 
    plantName :: String,
    plantFlowerColour :: FlowerColour,
    plantEnergy :: Int,
    plantGenome :: [Bool]
  } deriving (Show, Generic)

instance Serialize Plant

instance Agent Plant where
  agentId = plantName
  isAlive plant = plantEnergy plant > 0

instance Record Plant where key = agentId

data FlowerColour = Red | Orange | Yellow | Violet | Blue
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Serialize FlowerColour

colourCode :: Code FlowerColour Bool
colourCode = mkGrayCode [Red .. Blue]

buildPlant :: String -> [Bool] -> Plant
buildPlant name g = Plant name colour 10 g
  where colour = fromMaybe Red c
        (c, _) = decodeNext colourCode g

instance Reproductive Plant where
  type Base Plant = Bool
  recombine a b = 
    withProbability 0.1 randomCrossover (plantGenome a, plantGenome b) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build name g = Just $ buildPlant name g

tryMating :: [Plant] -> StateT (SimpleUniverse a) IO [Plant]
tryMating (me:other:_) = do
  name <- genName
  (Just baby) <- liftIO $ evalRandIO (makeOffspring me other name)
  writeToLog $ 
    plantName me ++ " and " ++ plantName other ++
      " gave birth to " ++ name ++ ", with " ++ 
       show (plantFlowerColour baby) ++ " flowers"
  return [deductMatingEnergy me, deductMatingEnergy other, baby]
tryMating x = return x -- need two agents to mate

deductMatingEnergy :: Plant -> Plant
deductMatingEnergy p = p {plantEnergy=plantEnergy p - 1}

