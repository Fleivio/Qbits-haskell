module BB84.Person(Person(Person, readBasis, obsRes), PersonRef(PersonRef), discardDifferentBasis, printObservations, mkPersonRef, getPerson) where

import Reference
import Data.IORef (IORef, newIORef, readIORef)

data Person = Person
  { readBasis :: [Base],
    obsRes :: [Bool]
  }

newtype PersonRef = PersonRef (IORef Person)

mkPersonRef :: IO PersonRef
mkPersonRef = do
  a <- newIORef $ Person [] []
  return $ PersonRef a

discardDifferentBasis :: Person -> Person -> [Bool]
discardDifferentBasis alice bob = [ bit | (a, b, bit) <- zipQbits, a == b ]
  where zipQbits = zip3 (readBasis alice) (readBasis bob) (obsRes alice)

printObservations :: Person -> IO ()
printObservations person = do
  putStrLn $ "Obs :" ++ show qbits
  where
    qbits = zip (readBasis person) (obsRes person)

getPerson :: PersonRef -> IO Person
getPerson (PersonRef p) = do readIORef p