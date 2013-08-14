import Tutorial.Chapter5.Rock (Rock(..))
import ALife.Creatur.Universe (addAgent, mkSimpleUniverse)
import Control.Monad.State.Lazy (evalStateT)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter5" "chapter5" 100000

  let a = Rock "Rocky" 42
  evalStateT (addAgent a) u

  let b = Rock "Roxie" 99
  evalStateT (addAgent b) u

  return ()

