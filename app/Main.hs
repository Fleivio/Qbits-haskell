module Main (main) where

import QuantumValue
import Operators
import ProbabilityAmplitude
import BoolOperators


ket_01_bra :: QV (Bool, Bool)
ket_01_bra = bra_0_ket &* bra_1_ket

ket_01_11_bra :: QV (Bool, Bool)
ket_01_11_bra = normalize $ toQv [(False, 1), (True, 1) ] &* toQv [(True, 1)]

genQv :: PA -> PA -> QV Bool
genQv a b = normalize $ toQv [(False, a), (True, b)]

genQv2 :: PA -> PA -> PA -> PA -> QV (Bool, Bool)
genQv2 a b c d = normalize $ toQv [((False, False), a),
                      ((False, True), b),
                      ((True, False), c),
                      ((True, True), d) ] 

print_tests :: IO ()
print_tests = do
     putStrLn $ "|01> = " ++ braketConvert ket_01_bra
     putStrLn $ "|01> + |11> = " ++ braketConvert ket_01_11_bra
     putStrLn $ " 1 |0> + 2 |1> = " ++ braketConvert (genQv 1 2)

print_gate_tests :: IO ()
print_gate_tests = do
     putStrLn $ "X |0> = " ++ braketConvert (qApp xGate (genQv 1 0))
     putStrLn $ "X |1> = " ++ braketConvert (qApp xGate (genQv 0 1))
     putStrLn $ "H |0> = " ++ braketConvert (qApp hGate (genQv 1 0))
     putStrLn $ "Y |0> = " ++ braketConvert (qApp yGate (genQv 1 0))
     putStrLn $ "CNOT |11> = " ++ braketConvert (qApp cnot (genQv2 0 0 0 1))
     putStrLn $ "bell phiP = " ++ braketConvert (bra_phi_p_ket)
     putStrLn $ "bell phiM = " ++ braketConvert (bra_phi_m_ket)
     putStrLn $ "bell psiP = " ++ braketConvert (bra_psi_p_ket)
     putStrLn $ "bell psiM = " ++ braketConvert (bra_psi_m_ket)
     putStrLn $ "TOFF |110> = " ++ braketConvert (qApp toffoli ((genQv 0 1) &* (genQv 0 1) &* (genQv 1 0) ))

main :: IO ()
main = print_gate_tests