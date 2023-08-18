module BoolOperators (xGate, yGate, zGate, hGate, cnotGate, toffGate, idGate,
 entangle,
 bra_0_ket, bra_1_ket, bra_phi_m_ket, bra_phi_p_ket, bra_psi_p_ket, bra_psi_m_ket, observeAtBasis, vGate, vtGate, toffGate',
 ObsBasis(..), deutsch, adder, adderRef) where

import Quantum.Operators
import Quantum.Value
import Reference.Reference
import Reference.Observation
import Virtual.VirtualValue
import Virtual.Adaptor


data ObsBasis = X | Z deriving (Show, Eq)


boolToQv :: Bool -> QV Bool
boolToQv b = mkQV [(b, 1)]

xGate :: Qop Bool Bool
xGate = qop [((False, True), 1),
             ((True, False), 1)]
            "X"

yGate :: Qop Bool Bool
yGate = qop [((False, True), 0 `addPA` (-1)),
             ((True, False), 0 `addPA` 1)]
            "Y"

zGate :: Qop Bool Bool
zGate = qop [((False, False), 1),
             ((True, True), -1)]
            "Z"

hGate :: Qop Bool Bool
hGate = qop [((False, False), 1),
             ((False, True), 1),
             ((True, False), 1),
             ((True, True), -1) ]
            "H"

vGate :: Qop Bool Bool
vGate = qop [((False, False), 1),
            ((True, True), 0 `addPA` 1)]
            "V"

vtGate :: Qop Bool Bool
vtGate = qop [((False, False), 1),
              ((True, True), 0 `addPA` (-1))]
            "Vt"

idGate :: Qop Bool Bool
idGate = qop [((True, True), 1),
              ((False, False), 1)]
            "Id"

cnotGate :: Qop (Bool, Bool) (Bool, Bool)
cnotGate = cqop id xGate "Cnot"

toffGate :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffGate = cqop (uncurry (&&)) xGate "Toffoli"

entangle :: QV Bool -> QV Bool -> QV (Bool, Bool)
entangle q1 q2 = qApp cnotGate (qApp hGate q1 &* q2)

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

toffGate' :: (Basis na, Basis u) => Virt ((Bool, Bool), Bool) na u -> IO()
toffGate' triple = 
    do
        app1 hGate b
        app1 cv mb
        app1 cnotGate tm
        app1 cvt mb
        app1 cnotGate tm
        app1 cv tb
        app1 hGate b
    where
        b = virtFromV triple ad_triple3
        mb = virtFromV triple ad_triple23
        tm = virtFromV triple ad_triple12
        tb = virtFromV triple ad_triple13
        cv = cqop id vGate "Cv"
        cvt = cqop id vtGate "Cvt"

deutsch :: (Bool -> Bool) -> IO()
deutsch f = 
    do 
        inpr <- mkQR (bra_0_ket &* bra_1_ket)
        let both = virtFromR inpr
            top = virtFromV both ad_pair1
            bot = virtFromV both ad_pair2
            uf = cqop f xGate "Uf"
        app1 hGate top
        app1 hGate bot 
        app1 uf both
        app1 hGate top
        topV <- observeVV top
        putStr (if topV then "Balanced" else "Constant")

adder :: QV Bool -> QV Bool -> QV Bool -> IO (QV Bool, QV Bool)
adder inc x y =
    let outc = bra_0_ket                -- carry out
        vals = x &* y &* inc &* outc    -- (x, y, inc, out)
    in do 
        r <- mkQR vals
        let v = virtFromR r
            vxyo = virtFromV v ad_quad124
            vxy = virtFromV v ad_quad12
            vyio = virtFromV v ad_quad234
            vyi = virtFromV v ad_quad23
            vio = virtFromV v ad_quad34
        app1 toffGate vxyo
        app1 cnotGate vxy
        app1 toffGate vyio
        app1 cnotGate vyi
        app1 cnotGate vxy
        (sumR, carryOut) <- observeVV vio
        putStrLn $ qvToString x ++ " +. " ++ qvToString y ++ " +. " ++ qvToString inc
        putStrLn $ "Sum = " ++ show sumR ++ "\nCarry = " ++ show carryOut
        return (boolToQv carryOut, boolToQv sumR)

adderRef :: QR Bool -> QR Bool -> QR Bool -> QR Bool -> QR Bool -> IO()
adderRef (QR cin') (QR x') (QR y') (QR cout') (QR s') = do
    cin <- readIORef cin'
    x   <- readIORef x'
    y   <- readIORef y'
    (cout, s) <- adder cin x y
    writeIORef s' s
    writeIORef cout' cout
    
-- Constants

bra_1_ket :: QV Bool
bra_1_ket = mkQV [(True, 1)]

bra_0_ket :: QV Bool
bra_0_ket = mkQV [(False, 1)]

bra_phi_p_ket :: QV (Bool, Bool)
bra_phi_p_ket = entangle bra_0_ket bra_0_ket

bra_phi_m_ket :: QV (Bool, Bool)
bra_phi_m_ket = entangle bra_1_ket bra_0_ket

bra_psi_p_ket :: QV (Bool, Bool)
bra_psi_p_ket = entangle bra_0_ket bra_1_ket

bra_psi_m_ket :: QV (Bool, Bool)
bra_psi_m_ket = entangle bra_1_ket bra_1_ket