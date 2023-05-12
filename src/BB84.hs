module BB84 (bb84, bb84Interference, bb84Discard, prsObservations) where

import BoolOperators
import Operators
import Reference
import System.Random

genBitSet :: Int -> IO [Bool]
genBitSet n = do
  take n . randomRs (True, False) <$> newStdGen

genBaseSet :: Int -> IO [Base]
genBaseSet n = do
  indexes <- take n . randomRs (0, 1) <$> newStdGen
  let values = [X, Z]
  return $ take n [values !! i | i <- indexes]

prepareQbits :: [Bool] -> [Base] -> IO [QR Bool]
prepareQbits bits basis = mapM mkQR (zipWith prepareQbit bits basis)
  where
    prepareQbit bit base = case base of
      Z -> if bit then bra_1_ket else bra_0_ket
      X ->
        if bit
          then qApp hGate bra_1_ket
          else qApp hGate bra_0_ket

data Person = Person
  { name :: String,
    readBasis :: [Base],
    obsRes :: [Bool],
    original :: [QR Bool]
  }

prsObservations :: Person -> IO ()
prsObservations person = do
  putStrLn $ name person
  putStrLn $ "Obs :" ++ show qbits
  where
    qbits = zip (readBasis person) (obsRes person)

initAlice :: Int -> IO Person
initAlice n = do
  aBases <- genBaseSet n
  aObs <- genBitSet n
  aQbits <- prepareQbits aObs aBases
  let alice = Person "Alice" aBases aObs aQbits
  return alice

receiveQbits :: String -> [QR Bool] -> IO Person
receiveQbits pName inputQbits = do
  bBases <- genBaseSet (length inputQbits)
  let bObsIO = sequence [observeAtBasis base qref | (base, qref) <- zip bBases inputQbits]
  bObs <- bObsIO
  let bob = Person pName bBases bObs inputQbits
  return bob

bb84 :: Int -> IO (Person, Person)
bb84 n = do
  alice <- initAlice n
  bob <- receiveQbits "Bob" (original alice)
  return (alice, bob)

bb84Interference :: Int -> IO (Person, Person, Person)
bb84Interference n = do
  alice <- initAlice n
  trudy <- receiveQbits "Trudy" (original alice)
  bob <- receiveQbits "Bob" (original alice)
  return (alice, trudy, bob)

bb84Discard :: Person -> Person -> [Bool]
bb84Discard alice bob = [ bit | (a, b, bit) <- zipQbits, a == b ]
  where zipQbits = zip3 (readBasis alice) (readBasis bob) (obsRes alice)