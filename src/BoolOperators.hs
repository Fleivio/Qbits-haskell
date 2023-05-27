module BoolOperators (xGate, yGate, zGate, hGate, cnot, toffoli, idGate,
 entangle,
 bra_0_ket, bra_1_ket, bra_phi_m_ket, bra_phi_p_ket, bra_psi_p_ket, bra_psi_m_ket, observeAtBasis, vGate, vtGate, toffoli',
 ObsBasis(..), deutsch) where

import Basic.Operators
import Basic.QuantumValue
import Basic.Reference
import Basic.Observation
import VirtualValues.VirtualValue
import VirtualValues.Adaptor
import Basic.Basis

import Data.Complex

data ObsBasis = X | Z deriving (Show, Eq)


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

vGate :: Qop Bool Bool
vGate = qop [((False, False), 1),
            ((True, True), 0 :+ 1)]

vtGate :: Qop Bool Bool
vtGate = qop [((False, False), 1),
              ((True, True), 0 :+ (-1))]

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

toffoli' :: (Basis na, Basis u) => Virt (Bool, Bool, Bool) na u -> IO()
toffoli' triple = 
    do
        app1 hGate b
        app1 cv mb
        app1 cnot tm
        app1 cvt mb
        app1 cnot tm
        app1 cv tb
        app1 hGate b
    where
        b = virtFromV triple ad_triple3
        mb = virtFromV triple ad_triple23
        tm = virtFromV triple ad_triple12
        tb = virtFromV triple ad_triple13
        cv = cqop id vGate
        cvt = cqop id vtGate

deutsch :: (Bool -> Bool) -> IO()
deutsch f = 
    do 
        inpr <- mkQR (bra_0_ket &* bra_1_ket)
        let both = virtFromR inpr
            top = virtFromV both ad_pair1
            bot = virtFromV both ad_pair2
            uf = cqop f xGate
        app1 hGate top
        app1 hGate bot 
        app1 uf both
        app1 hGate top
        topV <- observeVV top
        putStr (if topV then "Balanced" else "Constant")

adder :: QV Bool -> QV Bool -> QV Bool -> IO()
adder inc x y = 
    let cout = bra_0_ket
        qval = x &* y &* inc &* cout 
    in do 
        r <- mkQR qval
        let v = virtFromV (virtFromR r) ad_quad
            vxyo = virtFromV v ad_quad124
            vxy = virtFromV v ad_quad12
            vyio = virtFromV v ad_quad234
            vyi = virtFromV v ad_quad23
        app toffoli vxyo

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