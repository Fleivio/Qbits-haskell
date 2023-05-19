module Main (main) where

import BB84.BB84 ( bb84Interf, verifyInterference )
import BB84.Person

main :: IO()
main = do
  (alice, trudy, bob) <- bb84Interf 10
  printObservations alice
  printObservations trudy
  printObservations bob
  str <- verifyInterference alice bob
  putStrLn str