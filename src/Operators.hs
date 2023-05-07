module Operators (hGate, xGate, yGate, zGate, qop, qApp, cnot, entangle) where

import QuantumValue
import Data.Complex
import Data.Map
import Basis
import ProbabilityAmplitude

data Qop a b = Qop (Map (a, b) PA)

qop :: (Basis a, Basis b) => [((a,b), PA)] -> Qop a b
qop = Qop . fromList

qApp :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
qApp (Qop mp) qval = toQv [ (b, probB b) | b <- basis ]
    where 
        probB b = sum [ probToMap (a, b) * probOriginal a | a <- basis ]
        probOriginal = getProb qval
        probToMap = getProb mp

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


cqop :: (Basis a, Basis b) => (a -> Bool) -> Qop b b -> Qop (a, b) (a, b)
cqop enable (Qop u) = qop ( unchangeCase ++ changeCase )
    where
        unchangeCase = [( ((a, b) , (a, b)), 1 ) | (a, b) <- basis, not (enable a)] -- enable = false
        changeCase = [( ((a, b1) , (a, b2)), getProb u (b1, b2) ) | a <- basis, enable a, b1 <- basis, b2 <- basis] -- enable = true

cnot :: Qop (Bool, Bool) (Bool, Bool)
cnot = cqop id xGate

toffoli :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli = cqop (uncurry (&&)) xGate

entangle :: QV Bool -> QV Bool -> QV (Bool, Bool) 
entangle q1 q2 = qApp cnot ((qApp hGate q1) &* q2)

norm :: Basis a => QV a -> Double
norm v = sqrt . sum $ probs
    where probs = Prelude.map squareModulus (Prelude.map snd (toList v))

normalize :: Basis a => QV a -> QV a 
normalize qval = Data.Map.map (c*) qval
    where
        c = (1 / norm qval :+ 0)
