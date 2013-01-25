import Tutorial.Example1.Rock (Rock(..))
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import Control.Monad.State.Lazy (evalStateT)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Example1" "example1" 100000

  let a = Rock "Rocky" 42
  evalStateT (addAgent a) u

  let b = Rock "Roxie" 99
  evalStateT (addAgent b) u

  return ()

