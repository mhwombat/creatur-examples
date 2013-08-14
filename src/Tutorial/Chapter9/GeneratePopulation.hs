import Tutorial.Chapter9.Bug (Bug, buildBug)
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import ALife.Creatur.Genetics.BRGCBool (DiploidReader,
  runDiploidReader)
import Control.Monad.State.Lazy (evalStateT)
import Data.Maybe (catMaybes)
import System.Random (randoms, getStdGen)

buildBugs :: [String] -> DiploidReader [Bug]
buildBugs names = do
  bugs <- mapM (buildBug True) names
  return . catMaybes $ bugs

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter9" "chapter9" 100000

  -- Create some Bugs and save them in the population directory.
  let names = ["Bugsy", "Mel", "Flo", "Buzz"]

  r1 <- getStdGen -- source of random genes
  r2 <- getStdGen -- source of random genes

  let g1 = randoms r1
  let g2 = randoms r2

  let agents = runDiploidReader (buildBugs names) (g1, g2)
  mapM_ (\b -> evalStateT (addAgent b) u) agents

  return ()

