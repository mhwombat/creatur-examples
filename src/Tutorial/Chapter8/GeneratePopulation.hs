import Tutorial.Chapter8.Bug (Sex(..), BugColour(..), buildBug)
import ALife.Creatur.Universe (store, mkSimpleUniverse)
import ALife.Creatur.Genetics.BRGCBool (put, runWriter,
  runDiploidReader)
import Control.Monad.State.Lazy (evalStateT)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter8" "chapter8"

    -- Create some Bugs and save them in the population directory.
  let g1 = runWriter (put Male >> put Green)
  let (Right b1) = runDiploidReader (buildBug "Bugsy") (g1,g1)
  evalStateT (store b1) u

  let g2 = runWriter (put Male >> put Purple) 
  let (Right b2) = runDiploidReader (buildBug "Mel") (g2,g2)
  evalStateT (store b2) u

  let g3 = runWriter (put Female >> put Green)
  let (Right b3) = runDiploidReader (buildBug "Flo") (g3, g3)
  evalStateT (store b3) u

  let g4 = runWriter (put Male >> put Purple)
  let (Right b4) = runDiploidReader (buildBug "Buzz") (g4, g4)
  evalStateT (store b4) u

