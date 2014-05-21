import Tutorial.Chapter5.Rock (Rock(..))
import ALife.Creatur.Universe (store, mkSimpleUniverse)
import Control.Monad.State.Lazy (evalStateT)

main :: IO ()
main = do
  let u = mkSimpleUniverse "Chapter5" "chapter5"

  let a = Rock "Rocky" 42
  evalStateT (store a) u

  let b = Rock "Roxie" 99
  evalStateT (store b) u

  return ()

