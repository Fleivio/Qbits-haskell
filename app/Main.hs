module Main (main) where

import QuantumValue
import Operators
import ProbabilityAmplitude

ket_01_bra :: QV (Bool, Bool)
ket_01_bra = toQv [(False, 1)] &* toQv [(True, 1)]

ket_01_11_bra :: QV (Bool, Bool)
ket_01_11_bra = toQv [(False, 1/sqrt 2), (True, 1/sqrt 2) ] &* toQv [(True, 1)]

genQv :: PA -> PA -> QV Bool
genQv a b = toQv [(False, a), (True, b)]

bell_01 :: QV (Bool, Bool)
bell_01 = toQv [ ((True, True), 1 / sqrt 2), ((False, False), 1 / sqrt 2) ]

test_3_base :: QV ((Bool, Bool), Bool)
test_3_base = toQv [ (((True, False), True), 1), (((False, False), (True)), 1) ]


print_tests :: IO ()
print_tests = do
     putStrLn $ "|01> = " ++ braketConvert ket_01_bra
     putStrLn $ "|01> + |11> = " ++ braketConvert ket_01_11_bra
     putStrLn $ "|00> + |11> = " ++ braketConvert bell_01
     putStrLn $ " 1 |0> + 2 |1> = " ++ braketConvert (genQv 1 2)
     putStrLn $ " teste = " ++ braketConvert test_3_base

print_gate_tests :: IO ()
print_gate_tests = do
     putStrLn $ "X |0> = " ++ braketConvert (qApp xGate (genQv 1 0))
     putStrLn $ "X |1> = " ++ braketConvert (qApp xGate (genQv 0 1))
     putStrLn $ "H |0> = " ++ braketConvert (qApp hGate (genQv 1 0))
     putStrLn $ "Y |0> = " ++ braketConvert (qApp yGate (genQv 1 0))

main :: IO ()
main = print_tests