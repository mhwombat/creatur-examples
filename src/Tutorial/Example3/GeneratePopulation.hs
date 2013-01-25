import Tutorial.Example3.Bug (Sex(..), BugColour(..),
  sexCode, colourCode, buildBug)
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import ALife.Creatur.Genetics.Code (encode)
import Control.Monad.State.Lazy (evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Example3" "example3" 100000

  -- Create some Bugs and save them in the population directory.
  let g1 = fromJust (encode sexCode Male) ++ 
              fromJust (encode colourCode Green)
  let b1 = buildBug "Bugsy" (g1,g1)
  evalStateT (addAgent b1) u

  let g2 = fromJust (encode sexCode Male) ++ 
              fromJust (encode colourCode Purple)
  let b2 = buildBug "Mel" (g2,g2)
  evalStateT (addAgent b2) u

  let g3 = fromJust (encode sexCode Female) ++ 
              fromJust (encode colourCode Green)
  let b3 = buildBug "Zelda" (g3, g3)
  evalStateT (addAgent b3) u

  let g4 = fromJust (encode sexCode Female) ++ 
              fromJust (encode colourCode Purple)
  let b4 = buildBug "Betty" (g4, g4)
  evalStateT (addAgent b4) u

  return ()

