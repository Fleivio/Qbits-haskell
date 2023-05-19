module BoolOperators (xGate, yGate, zGate, hGate, cnot, toffoli, idGate,
 entangle,
 bra_0_ket, bra_1_ket, bra_phi_m_ket, bra_phi_p_ket, bra_psi_p_ket, bra_psi_m_ket, observeAtBasis) where

import Basic.Operators
import Basic.QuantumValue
import Basic.Reference
import Basic.Observation

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
entangle q1 q2 = qApp cnot (qApp hGate q1 &* q2)

observeAtBasis :: ObsBasis -> QR Bool -> IO Bool
observeAtBasis Z qr =
    do
        qrApplyOp idGate qr
        observeR qr
observeAtBasis X qr =
    do
        qrApplyOp hGate qr
        a <- observeR qr
        qrApplyOp hGate qr
        return a

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