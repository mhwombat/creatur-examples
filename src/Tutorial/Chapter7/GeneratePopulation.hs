import Tutorial.Chapter7.Plant (FlowerColour(..), buildPlant)
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import ALife.Creatur.Genetics.BRGCBool (write, runReader)
import Control.Monad.State.Lazy (evalStateT)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter7" "chapter7" 100000

  -- Create some plants and save them in the population directory.
  let g1 = write Red
  let (Right p1) = runReader (buildPlant "Rose") g1
  evalStateT (addAgent p1) u

  let g2 = write Yellow
  let (Right p2) = runReader (buildPlant "Sunny") g2
  evalStateT (addAgent p2) u

  let g3 = write Violet
  let (Right p3) = runReader (buildPlant "Vi") g3
  evalStateT (addAgent p3) u

  return ()

