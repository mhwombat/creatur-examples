import ALife.Creatur.Genetics.BRGCBool (runReader, write)
import ALife.Creatur.Universe          (mkSimpleUniverse, store)
import Control.Monad.State.Lazy        (evalStateT)
import Tutorial.Chapter7.Plant         (FlowerColour (..), buildPlant)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter7" "chapter7"

  -- Create some plants and save them in the population directory.
  let g1 = write Red
  let (Right p1) = runReader (buildPlant "Rose") g1
  evalStateT (store p1) u

  let g2 = write Yellow
  let (Right p2) = runReader (buildPlant "Sunny") g2
  evalStateT (store p2) u

  let g3 = write Violet
  let (Right p3) = runReader (buildPlant "Vi") g3
  evalStateT (store p3) u

  return ()

