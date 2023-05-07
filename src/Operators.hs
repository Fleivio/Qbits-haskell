module Operators (qop, qApp, cqop, normalize, Qop) where

import QuantumValue
import Data.Complex
import Data.Map as Map (Map, fromList, toList, map)
import Basis
import ProbabilityAmplitude
import Prelude as P

data Qop a b = Qop (Map (a, b) PA)

qop :: (Basis a, Basis b) => [((a,b), PA)] -> Qop a b
qop = Qop . fromList

cqop :: (Basis a, Basis b) => (a -> Bool) -> Qop b b -> Qop (a, b) (a, b)
cqop enable (Qop u) = qop ( unchangeCase ++ changeCase )
    where
        unchangeCase = [( ((a, b) , (a, b)), 1 ) | (a, b) <- basis, not (enable a)] -- enable = false
        changeCase = [( ((a, b1) , (a, b2)), getProb u (b1, b2) ) | a <- basis, enable a, b1 <- basis, b2 <- basis] -- enable = true

qApp :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
qApp (Qop mp) qval = toQv [ (b, probB b) | b <- basis ]
    where 
        probB b = sum [ probToMap (a, b) * probOriginal a | a <- basis ]
        probOriginal = getProb qval
        probToMap = getProb mp

norm :: QV a -> Double
norm v = sqrt . sum $ probs
    where probs = P.map squareModulus (P.map snd (toList v))

normalize :: QV a -> QV a 
normalize qval = Map.map (c*) qval
    where
        c = (1 / norm qval :+ 0)
