module Main (main) where

import BB84

bb84test :: IO ()
bb84test = do
  (a, b) <- bb84 10
  prsObservations a
  prsObservations b
  let key = bb84Discard a b
  putStrLn $ "Chave: " ++ show key

bb84InterferenceTest :: IO()
bb84InterferenceTest = do
  (a, t, b) <- bb84Interference 10
  prsObservations a
  prsObservations t
  prsObservations b
  let key = bb84Discard a b
  putStrLn $ "Chave: " ++ show key

main :: IO ()
main = bb84InterferenceTest
