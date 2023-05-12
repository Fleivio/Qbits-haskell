module BB84 (bb84, bb84Discard, printObservations) where

import BoolOperators
import Operators
import Reference
import System.Random
import Control.Concurrent

genBitSet :: Int -> IO [Bool]
genBitSet n = do
  take n . randomRs (True, False) <$> newStdGen

genBaseSet :: Int -> IO [Base]
genBaseSet n = do
  indexes <- take n . randomRs (0, 1) <$> newStdGen
  let values = [X, Z]
  return $ take n [values !! i | i <- indexes]

prepareQbit :: Bool -> Base -> IO(QR Bool)
prepareQbit False Z = mkQR $ bra_0_ket
prepareQbit True Z = mkQR $ bra_1_ket
prepareQbit False X = mkQR $ qApp hGate bra_0_ket
prepareQbit True X = mkQR $ qApp hGate bra_1_ket

prepareQbits :: Chan (QR Bool) -> [Bool] -> [Base] -> IO ()
prepareQbits _ [] _ = pure ()
prepareQbits _ _ [] = pure ()
prepareQbits channel (bit:bits) (base : basis) = do
  qr <- prepareQbit bit base
  writeChan channel qr
  putStrLn $ "Alice gerou: " ++ show bit
  prepareQbits channel bits basis

printObservations :: Person -> IO ()
printObservations person = do
  putStrLn $ "Obs :" ++ show qbits
  where
    qbits = zip (readBasis person) (obsRes person)

bb84Discard :: Person -> Person -> [Bool]
bb84Discard alice bob = [ bit | (a, b, bit) <- zipQbits, a == b ]
  where zipQbits = zip3 (readBasis alice) (readBasis bob) (obsRes alice)

data Person = Person
  { readBasis :: [Base],
    obsRes :: [Bool]
  }

initQbits :: Chan (QR Bool) -> Chan Person -> Int -> IO ()
initQbits channel pChan n = do
  aBases <- genBaseSet n
  aObs <- genBitSet n
  _ <- forkIO(prepareQbits channel aObs aBases)
  let alice = Person aBases aObs 
  writeChan pChan alice


receiveQbits :: Chan (QR Bool) -> Chan Person -> Int -> IO()
receiveQbits channel pChan size = do
  bBases <- genBaseSet size
  bObs <- readFromChannel size bBases
  let bob = Person bBases bObs
  writeChan pChan bob

  where readFromChannel sz baseSet
          | sz == 0 = return []
          | otherwise = do
            input <- readChan channel
            res <- observeAtBasis (head baseSet) input
            putStrLn $ "Bob leu: " ++ show res
            resTail <- readFromChannel (sz - 1) (tail baseSet)
            return $ res : resTail

bb84 :: Int -> IO (Person, Person)
bb84 n = do
  channel <- newChan
  pChan <- newChan
  _ <- forkIO(initQbits channel pChan n)
  _ <- forkIO(receiveQbits channel pChan n)
  alice <- readChan pChan
  bob <- readChan pChan 
  return (alice, bob)

-- bb84Interference :: Int -> IO (Person, Person, Person)
-- bb84Interference n = do
--   alice <- initQbits n
--   trudy <- receiveQbits "Trudy" (original alice)
--   bob <- receiveQbits "Bob" (original alice)
--   return (alice, trudy, bob)

