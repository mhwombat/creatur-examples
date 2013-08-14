import Tutorial.Chapter8.Bug (Sex(..), BugColour(..), buildBug)
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import ALife.Creatur.Genetics.BRGCBool (put, runWriter,
  runDiploidReader)
import Control.Monad.State.Lazy (evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter8" "chapter8" 100000

    -- Create some Bugs and save them in the population directory.
  let g1 = runWriter (put Male >> put Green)
  let b1 = fromJust $ runDiploidReader (buildBug "Bugsy") (g1,g1)
  evalStateT (addAgent b1) u

  let g2 = runWriter (put Male >> put Purple) 
  let b2 = fromJust $ runDiploidReader (buildBug "Mel") (g2,g2)
  evalStateT (addAgent b2) u

  let g3 = runWriter (put Female >> put Green)
  let b3 = fromJust $ runDiploidReader (buildBug "Flo") (g3, g3)
  evalStateT (addAgent b3) u

  let g4 = runWriter (put Male >> put Purple)
  let b4 = fromJust $ runDiploidReader (buildBug "Buzz") (g4, g4)
  evalStateT (addAgent b4) u

  return ()

