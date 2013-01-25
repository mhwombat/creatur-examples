import Tutorial.Example2.Plant (FlowerColour(..), colourCode, buildPlant)
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import ALife.Creatur.Genetics.Code (encode)
import Control.Monad.State.Lazy (evalStateT)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Example2" "example2" 100000

  -- Create some plants and save them in the population directory.
  let g1 = fromJust (encode colourCode Red)
  let f1 = buildPlant "Rose" g1
  evalStateT (addAgent f1) u

  let g2 = fromJust (encode colourCode Yellow)
  let f2 = buildPlant "Sunny" g2
  evalStateT (addAgent f2) u

  let g3 = fromJust (encode colourCode Violet)
  let f3 = buildPlant "Vi" g3
  evalStateT (addAgent f3) u

  return ()

