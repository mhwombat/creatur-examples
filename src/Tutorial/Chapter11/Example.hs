{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances,
    TypeFamilies #-}
import Prelude hiding (read)
import ALife.Creatur.Genetics.BRGCWord8 (Genetic, Reader, put, get,
  putRawWord8, getRawWord8, write, read)
import Control.Applicative ((<$>), (<*>))
import Data.Bits
import Data.Word (Word8)
import GHC.Generics (Generic)

--
-- This example shows how the default implementation of Genetic is
-- usually sufficient, even for a complex data structure.
-- 
data Colour = Green | Purple
  deriving (Show, Eq, Enum, Bounded, Generic)
instance Genetic Colour

data ComplexGene = A | B Colour | C Word8 | D Bool Char | E [ComplexGene]
  deriving (Show, Eq, Generic)

instance Genetic ComplexGene

--
-- This is an example of a custom implementation of Genetic. This
-- implementation uses the default implementations of Genetic for Colour
-- and Bool, showing that it is possible to mix and match.
--

data CustomGene = F Colour | G Bool
  deriving (Show, Eq, Generic)

instance Genetic CustomGene where
  put (F c) = putRawWord8 7 >> put c
  put (G b) = putRawWord8 8 >> put b
  get = do
    x <- getRawWord8
    case x of
      (Just 7) -> do
        c <- get :: Reader (Maybe Colour)
        return . fmap F $ c
      (Just 8) -> do
        b <- get :: Reader (Maybe Bool)
        return . fmap G $ b
      _      -> return Nothing

--
-- In this example, we store three boolean values in a Word8 value
-- to reduce the amount of storage required.
--
      
data CustomGene2 = H Bool Bool Bool
  deriving (Show, Eq, Generic)

instance Genetic CustomGene2 where
  put (H x y z) = putRawWord8 (x' + y' + z')
    where x' = (4 *) . fromIntegral . fromEnum $ x :: Word8
          y' = (2 *) . fromIntegral . fromEnum $ y :: Word8
          z' = fromIntegral . fromEnum $ z :: Word8
  get = do
    w <- getRawWord8 :: Reader (Maybe Word8)
    let x = fmap (flip testBit 2) w :: Maybe Bool
    let y = fmap (flip testBit 1) w :: Maybe Bool
    let z = fmap (flip testBit 0) w :: Maybe Bool
    return $ H <$> x <*> y <*> z

test :: (Eq x, Show x, Genetic x) => x -> IO ()
test x = do
  putStrLn $ "wrote gene: " ++ show x
  let dna = write x
  putStrLn $ "dna=" ++ show dna
  let x2 = read dna
  putStrLn $ "read: " ++ show x2
  if x2 == Just x
     then putStrLn "SUCCESS"
     else putStrLn "FAILURE"

main :: IO ()
main = do
  putStrLn "Example of complex gene"
  test (E [ A, B Purple, C 7, D True 'a', E []])

  putStrLn "\nExample of a custom implementation of Genetic"
  test (F Green)

  putStrLn "\nAnother example of a custom implementation of Genetic"
  test (H True False True)
