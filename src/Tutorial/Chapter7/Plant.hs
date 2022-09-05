{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Tutorial.Chapter7.Plant (Plant(..), FlowerColour(..),
  buildPlant, run) where

import ALife.Creatur                                        (Agent, agentId,
                                                             isAlive)
import ALife.Creatur.Database                               (Record, key)
import ALife.Creatur.Genetics.BRGCBool                      (Genetic, Reader,
                                                             Sequence, copy,
                                                             getWithDefault,
                                                             runReader)
import ALife.Creatur.Genetics.Recombination                 (mutatePairedLists,
                                                             randomCrossover,
                                                             randomCutAndSplice,
                                                             randomOneOfPair,
                                                             withProbability)
import ALife.Creatur.Genetics.Reproduction.SimplifiedSexual (Reproductive,
                                                             Strand, build,
                                                             makeOffspring,
                                                             recombine)
import ALife.Creatur.Universe                               (SimpleUniverse,
                                                             genName,
                                                             writeToLog)
import Control.Monad.IO.Class                               (liftIO)
import Control.Monad.Random                                 (evalRandIO)
import Control.Monad.State                                  (StateT)
import Data.Serialize                                       (Serialize)
import GHC.Generics                                         (Generic)

data Plant = Plant
  {
    plantName         :: String,
    plantFlowerColour :: FlowerColour,
    plantEnergy       :: Int,
    plantGenome       :: Sequence
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

buildPlant :: String -> Reader (Either [String] Plant)
buildPlant name = do
  g <- copy
  colour <- getWithDefault Red
  return . Right $ Plant name colour 10 g

instance Reproductive Plant where
  type Strand Plant = Sequence
  recombine a b =
    withProbability 0.1 randomCrossover (plantGenome a, plantGenome b) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build name = runReader (buildPlant name)

run :: [Plant] -> StateT (SimpleUniverse Plant) IO [Plant]
run (me:other:_) = do
  name <- genName
  (Right baby) <- liftIO $ evalRandIO (makeOffspring me other name)
  writeToLog $
    plantName me ++ " and " ++ plantName other ++
      " gave birth to " ++ name ++ ", with " ++
       show (plantFlowerColour baby) ++ " flowers"
  writeToLog $ "Me: " ++ show me
  writeToLog $ "Mate: " ++ show other
  writeToLog $ "Baby: " ++ show baby
  return [deductMatingEnergy me, deductMatingEnergy other, baby]
run _ = return [] -- need two agents to mate

deductMatingEnergy :: Plant -> Plant
deductMatingEnergy p = p {plantEnergy=plantEnergy p - 1}


