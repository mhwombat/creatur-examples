import Tutorial.Chapter10.Rock (Rock(..))
import Tutorial.Chapter10.Plant (FlowerColour(..), buildPlant)
import Tutorial.Chapter10.Bug (Sex(..), BugColour(..),
  buildBug)
import Tutorial.Chapter10.Martian (Martian(..))
import ALife.Creatur.Genetics.BRGCWord8 (put, runWriter, runReader,
  runDiploidReader)
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import Control.Monad.State.Lazy (evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter10" "chapter10" 100000

  -- Create some rocks and save them in the population directory.
  let a = FromRock $ Rock "Rocky" 0
  evalStateT (addAgent a) u

  let b = FromRock $ Rock "Roxie" 0
  evalStateT (addAgent b) u

  -- Create some plants and save them in the population directory.
  let gp1 = runWriter (put Red)
  let p1 = FromPlant . fromJust $ runReader (buildPlant "Rose") gp1
  evalStateT (addAgent p1) u

  let gp2 = runWriter (put Yellow)
  let p2 = FromPlant . fromJust $ runReader (buildPlant "Sunny") gp2
  evalStateT (addAgent p2) u

  let gp3 = runWriter (put Violet)
  let p3 = FromPlant . fromJust $ runReader (buildPlant "Vi") gp3
  evalStateT (addAgent p3) u

  -- Create some Bugs and save them in the population directory.
  let gb1 = runWriter (put Male >> put Green)
  let b1 = FromBug . fromJust $ runDiploidReader (buildBug "Bugsy") (gb1,gb1)
  evalStateT (addAgent b1) u

  let gb2 = runWriter (put Male >> put Purple) 
  let b2 = FromBug . fromJust $ runDiploidReader (buildBug "Mel") (gb2,gb2)
  evalStateT (addAgent b2) u

  let gb3 = runWriter (put Female >> put Green)
  let b3 = FromBug . fromJust $ runDiploidReader (buildBug "Flo") (gb3,gb3)
  evalStateT (addAgent b3) u

  let gb4 = runWriter (put Male >> put Purple)
  let b4 = FromBug . fromJust $ runDiploidReader (buildBug "Buzz") (gb4,gb4)
  evalStateT (addAgent b4) u

  let gb5 = runWriter (put Female >> put Green)
  let b5 = FromBug . fromJust $ runDiploidReader (buildBug "Betty") (gb5,gb5)
  evalStateT (addAgent b5) u

  let gb6 = runWriter (put Female >> put Green)
  let b6 = FromBug . fromJust $ runDiploidReader (buildBug "Anna") (gb6,gb6)
  evalStateT (addAgent b6) u

  return ()

