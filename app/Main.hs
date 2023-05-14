module Main (main) where

import BB84.BB84
import BB84.Person

main :: IO()
main = do
  (alice, trudy, bob) <- bb84Interf 15
  printObservations alice
  printObservations trudy
  printObservations bob
  let key = discardDifferentBasis alice bob
  print key   

-- main :: IO()
-- main = do
--   (alice, bob) <- bb84 10
--   printObservations alice
--   printObservations bob
--   let key = discardDifferentBasis alice bob
--   print key   