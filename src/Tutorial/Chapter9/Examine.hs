{-# LANGUAGE TypeFamilies #-}
import ALife.Creatur.Clock    (currentTime)
import ALife.Creatur.Database as D (DBRecord, Database)
import ALife.Creatur.Universe (Universe, addAgent, agentIds, getAgent,
                               mkSimpleUniverse)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State    (StateT, evalStateT)
import System.Environment     (getArgs)
import Tutorial.Chapter9.Bug  (Bug (..), BugColour (..), Sex (..))

getAndExamineAll
  :: (Database d, DBRecord d ~ Bug)
    => StateT (Universe c l d n x Bug) IO ()
getAndExamineAll = do
  names <- agentIds
  mapM_ getAndExamine names

getAndExamine
  :: (Database d, DBRecord d ~ Bug)
    => String -> StateT (Universe c l d n x Bug) IO ()
getAndExamine name = do
  a <- getAgent name
  case a of
    (Right agent) -> liftIO $ examine agent
    (Left msg)    -> liftIO $ putStrLn msg

examine :: Bug -> IO ()
examine a = do
  putStrLn $ bugName a ++ " is a " ++ show (bugSex a) ++ " "
    ++ show (bugColour a) ++ " bug with " ++ describeSpots (bugSpots a)
  putStrLn $ "Energy=" ++ show (bugEnergy a)
  putStrLn $ "Genome=" ++ show (bugGenome a)
  putStrLn ""

describeSpots :: [BugColour] -> String
describeSpots []     = "no spots"
describeSpots [a]    = show a ++ " spots"
describeSpots (a:bs) = show a ++ " and " ++ describeSpots bs

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter9" "chapter9" 100000
  t <- evalStateT currentTime u
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then do
      evalStateT (getAndExamineAll) u
    else do
      let name = head args
      evalStateT (getAndExamine name) u
