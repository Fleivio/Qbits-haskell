module Operators (hGate, xGate, yGate, zGate, qop, qApp) where

import QuantumValue
import Data.Complex
import Data.Map
import Basis
import ProbabilityAmplitude

data Qop a b = Qop ( Map (a, b) PA)

qop :: (Basis a, Basis b) => [((a,b), PA)] -> Qop a b
qop = Qop . fromList

qApp :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
qApp (Qop mp) qval = toQv [ (b, bF b) | b <- basis ]
    where
        bF b = sum [ getProb mp (a, b) * getProb qval a | a <- basis]

xGate :: Qop Bool Bool
xGate = qop [((False, True), 1),
             ((True, False), 1)]

yGate :: Qop Bool Bool
yGate = qop [((False, True), 0 :+ (-1)),
         ((True, False), 0 :+ 1)]

zGate :: Qop Bool Bool
zGate = qop [((False, False), 1),
         ((True, True) , 0)]

hGate :: Qop Bool Bool
hGate = qop [((False, False), 1),
          ((False, True), 1),
          ((True, False), 1),
          ((True, True), -1) ]