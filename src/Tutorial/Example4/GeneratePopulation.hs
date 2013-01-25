import Tutorial.Example4.Rock as R (Rock(..))
import Tutorial.Example4.Plant as P (FlowerColour(..), colourCode, 
  buildPlant)
import Tutorial.Example4.Bug as B (Sex(..), BugColour(..),
  sexCode, colourCode, buildBug)
import Tutorial.Example4.Martian (Martian(..))
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import ALife.Creatur.Genetics.Code (encode)
import Control.Monad.State.Lazy (evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Example4" "example4" 100000

  -- Create some rocks and save them in the population directory.
  let a = FromRock $ Rock "Rocky" 0
  evalStateT (addAgent a) u

  let b = FromRock $ Rock "Roxie" 0
  evalStateT (addAgent b) u

  -- Create some plants and save them in the population directory.
  let gp1 = fromJust (encode P.colourCode Red)
  let p1 = FromPlant $ buildPlant "Rose" gp1
  evalStateT (addAgent p1) u

  let gp2 = fromJust (encode P.colourCode Yellow)
  let p2 = FromPlant $ buildPlant "Sunny" gp2
  evalStateT (addAgent p2) u

  let gp3 = fromJust (encode P.colourCode Violet)
  let p3 = FromPlant $ buildPlant "Vi" gp3
  evalStateT (addAgent p3) u

  -- Create some Bugs and save them in the population directory.
  let gb1 = fromJust (encode sexCode Male) ++ 
              fromJust (encode B.colourCode Green)
  let b1 = FromBug $ buildBug "Bugsy" (gb1,gb1)
  evalStateT (addAgent b1) u

  let gb2 = fromJust (encode sexCode Male) ++ 
              fromJust (encode B.colourCode Purple)
  let b2 = FromBug $ buildBug "Mel" (gb2,gb2)
  evalStateT (addAgent b2) u

  let gb3 = fromJust (encode sexCode Female) ++ 
              fromJust (encode B.colourCode Green)
  let b3 = FromBug $ buildBug "Flo" (gb3, gb3)
  evalStateT (addAgent b3) u

  let gb4 = fromJust (encode sexCode Female) ++ 
              fromJust (encode B.colourCode Purple)
  let b4 = FromBug $ buildBug "Flo" (gb4, gb4)
  evalStateT (addAgent b4) u

  return ()

