{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main (main) where

import QuantumValue
import ProbabilityAmplitude

ket_01_bra :: QV (Bool, Bool)
ket_01_bra = toQv [(False, 1)] &* toQv [(True, 1)]

ket_01_11_bra :: QV (Bool, Bool)
ket_01_11_bra = toQv [(False, 1/sqrt 2), (True, 1/sqrt 2) ] &* toQv [(True, 1)]

genQv :: PA -> PA -> QV Bool
genQv a b = toQv [(False, a), (True, b)]

main :: IO ()
main = do
     putStrLn $ braketConvert ket_01_bra
     putStrLn $ braketConvert ket_01_11_bra
     putStrLn $ braketConvert $ genQv 1 2