module BoolOperators (xGate, yGate, zGate, hGate, cnot, toffoli, idGate,
 entangle,
 bra_0_ket, bra_1_ket, bra_phi_m_ket, bra_phi_p_ket, bra_psi_p_ket, bra_psi_m_ket) where

import Operators
import QuantumValue
import Data.Complex


xGate :: Qop Bool Bool
xGate = qop [((False, True), 1),
             ((True, False), 1)]

yGate :: Qop Bool Bool
yGate = qop [((False, True), 0 :+ (-1)),
             ((True, False), 0 :+ 1)]

zGate :: Qop Bool Bool
zGate = qop [((False, False), 1),
             ((True, True), -1)]

hGate :: Qop Bool Bool
hGate = qop [((False, False), 1),
             ((False, True), 1),
             ((True, False), 1),
             ((True, True), -1) ]

idGate :: Qop Bool Bool 
idGate = qop [((True, True), 1),
              ((False, False), 1)]

cnot :: Qop (Bool, Bool) (Bool, Bool)
cnot = cqop id xGate

toffoli :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli = cqop (uncurry (&&)) xGate

entangle :: QV Bool -> QV Bool -> QV (Bool, Bool) 
entangle q1 q2 = qApp cnot ((qApp hGate q1) &* q2)

-- Constants

bra_1_ket :: QV Bool
bra_1_ket = toQv [(True, 1)]

bra_0_ket :: QV Bool
bra_0_ket = toQv [(False, 1)]

bra_phi_p_ket :: QV (Bool, Bool)
bra_phi_p_ket = entangle bra_0_ket bra_0_ket

bra_phi_m_ket :: QV (Bool, Bool)
bra_phi_m_ket = entangle bra_1_ket bra_0_ket

bra_psi_p_ket :: QV (Bool, Bool)
bra_psi_p_ket = entangle bra_0_ket bra_1_ket

bra_psi_m_ket :: QV (Bool, Bool)
bra_psi_m_ket = entangle bra_1_ket bra_1_ket

-- genQv :: PA -> PA -> QV Bool
-- genQv a b = normalize $ toQv [(False, a), (True, b)]

-- genQv2 :: PA -> PA -> PA -> PA -> QV (Bool, Bool)
-- genQv2 a b c d = normalize $ toQv [((False, False), a),
--                       ((False, True), b),
--                       ((True, False), c),
--                       ((True, True), d) ]

-- _print_gate_tests :: IO ()
-- _print_gate_tests = do
--      putStrLn $ "X |0> = " ++ braketConvert (qApp xGate (genQv 1 0))
--      putStrLn $ "X |1> = " ++ braketConvert (qApp xGate (genQv 0 1))
--      putStrLn $ "H |0> = " ++ braketConvert (qApp hGate (genQv 1 0))
--      putStrLn $ "Y |0> = " ++ braketConvert (qApp yGate (genQv 1 0))
--      putStrLn $ "CNOT |11> = " ++ braketConvert (qApp cnot (genQv2 0 0 0 1))
--      putStrLn $ "bell phiP = " ++ braketConvert (bra_phi_p_ket)
--      putStrLn $ "bell phiM = " ++ braketConvert (bra_phi_m_ket)
--      putStrLn $ "bell psiP = " ++ braketConvert (bra_psi_p_ket)
--      putStrLn $ "bell psiM = " ++ braketConvert (bra_psi_m_ket)
--      putStrLn $ "TOFF |110> = " ++ braketConvert (qApp toffoli ((genQv 0 1) &* (genQv 0 1) &* (genQv 1 0) ))