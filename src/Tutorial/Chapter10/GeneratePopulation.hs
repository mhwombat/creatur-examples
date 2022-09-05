import ALife.Creatur.Genetics.BRGCWord8 (DiploidReader, Reader,
                                         runDiploidReader, runReader)
import ALife.Creatur.Universe           (mkSimpleUniverse, store)
import Control.Monad.State.Lazy         (evalStateT)
import Data.Either                      (rights)
import System.Random                    (getStdGen, newStdGen, randoms)
import Tutorial.Chapter10.Bug           (buildBug)
import Tutorial.Chapter10.Martian       (Martian (..))
import Tutorial.Chapter10.Plant         (buildPlant)
import Tutorial.Chapter10.Rock          (Rock (..))

buildPlants :: [String] -> Reader (Either [String] [Martian])
buildPlants names = do
  xs <- mapM (buildPlant True) names
  return . sequence $ map (fmap FromPlant) xs

buildBugs :: [String] -> DiploidReader [Martian]
buildBugs names = do
  xs <- mapM (buildBug True) names
  return . map FromBug . rights $ xs

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter10" "chapter10"

  -- Create some rocks and save them in the population directory.
  let rock1 = FromRock $ Rock "Rocky" 0
  evalStateT (store rock1) u

  let rock2 = FromRock $ Rock "Roxie" 0
  evalStateT (store rock2) u

  -- Create some plants and save them in the population directory.
  let plantNames = ["Rose", "Sunny", "Vi"]

  r <- newStdGen -- source of random genes
  let g = randoms r

  let (Right plants) = fst $ runReader (buildPlants plantNames) g
  mapM_ (\b -> evalStateT (store b) u) plants

  -- Note: The next part "hangs" for me. It didn't used to. And the
  -- same code works in Chapter 9. I haven't had a chance to debug
  -- the problem yet.

  -- Create some Bugs and save them in the population directory.
  let names = ["Bugsy", "Mel", "Flo", "Buzz"]

  r1 <- newStdGen -- source of random genes
  r2 <- getStdGen -- source of random genes

  let g1 = randoms r1
  let g2 = randoms r2

  let agents = runDiploidReader (buildBugs names) (g1, g2)
  mapM_ (\b -> evalStateT (store b) u) agents
