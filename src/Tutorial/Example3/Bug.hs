{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}
module Tutorial.Example3.Bug (Bug(..), Sex(..), BugColour(..), sexCode, 
  colourCode, buildBug, run) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.AgentNamer (genName)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Genetics.Gene (PairedGene, express, decodeAndExpress)
import ALife.Creatur.Genetics.Code (Code, mkGrayCode)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists, 
  randomCrossover, randomCutAndSplice, randomOneOfPair, 
  repeatWithProbability, withProbability)
import ALife.Creatur.Genetics.Reproduction.Sexual (Reproductive, Base, 
  produceGamete, build, makeOffspring)
import ALife.Creatur.Universe (SimpleUniverse)
import ALife.Creatur.Logger (writeToLog)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (StateT)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Bug = Bug
  { 
    bugName :: String,
    bugColour :: BugColour,
    bugSex :: Sex,
    bugEnergy :: Int,
    bugGenome :: ([Bool],[Bool])
  } deriving (Show, Generic)

instance Serialize Bug

instance Agent Bug where
  agentId = bugName
  isAlive bug = bugEnergy bug > 0

instance Record Bug where key = agentId

data BugColour = Green | Purple deriving (Show, Eq, Generic)
instance Serialize BugColour

data Sex = Male | Female deriving (Show, Eq, Generic)
instance Serialize Sex

colourCode :: Code BugColour Bool
colourCode = mkGrayCode [Green, Purple]

sexCode :: Code Sex Bool
sexCode = mkGrayCode [Male, Female]

instance PairedGene BugColour where
  express Green _       = Green
  express _ Green       = Green
  express Purple Purple = Purple

instance PairedGene Sex where
  express Male _        = Male
  express _ Male        = Male
  express Female Female = Female

buildBug :: String -> ([Bool], [Bool]) -> Bug
buildBug name g = Bug name colour sex 10 g
  where (s, g') = decodeAndExpress sexCode g
        (c, _) = decodeAndExpress colourCode g'
        sex = fromMaybe Female s
        colour = fromMaybe Green c

instance Reproductive Bug where
  type Base Bug = Bool
  produceGamete a = 
    repeatWithProbability 0.1 randomCrossover (bugGenome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build name g = Just $ buildBug name g

run :: [Bug] -> StateT (SimpleUniverse a) IO [Bug]
run (me:other:_) = do
  writeToLog $ agentId me ++ "'s turn" 
  if bugSex me == Female && bugSex other == Male
    then do
      name <- genName
      (Just baby) <- liftIO $ evalRandIO (makeOffspring me other name)
      writeToLog $ 
        bugName me ++ " and " ++ bugName other ++
          " gave birth to " ++ name ++ ", a " ++ 
          show (bugColour baby) ++ " " ++ show (bugSex baby) ++ " bug"
      return [deductMatingEnergy me, deductMatingEnergy other, baby]
    else return [me, other]
run x = return x -- need two agents to mate

deductMatingEnergy :: Bug -> Bug
deductMatingEnergy bug = bug {bugEnergy=bugEnergy bug - 1}


