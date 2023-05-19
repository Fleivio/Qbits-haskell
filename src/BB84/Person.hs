module BB84.Person(Person(Person, readBasis, obsRes), PersonRef(PersonRef), printObservations, mkPersonRef, getPerson) where

import Reference
import Data.IORef (IORef, newIORef, readIORef)

data Person = Person
  { readBasis :: [ObsBasis],
    obsRes :: [Bool]
  }

newtype PersonRef = PersonRef (IORef Person)

mkPersonRef :: IO PersonRef
mkPersonRef = do
  a <- newIORef $ Person [] []
  return $ PersonRef a

printObservations :: Person -> IO ()
printObservations person = do
  putStrLn $ "Obs :" ++ show qbits
  where
    qbits = zip (readBasis person) (obsRes person)

getPerson :: PersonRef -> IO Person
getPerson (PersonRef p) = do readIORef p