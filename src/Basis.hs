module Basis (boolBasis, qv, pr, qVal) where

import Data.Complex
import Data.Map

class (Eq a, Ord a) => Basis a where
    basis :: [a]

instance Basis Bool where
    basis = [False, True]


boolBasis :: IO()
boolBasis = print (basis :: [Bool])

type PA = Complex Double -- Probability Amplitude
type QV a = Map a PA     -- Quantum Value

-- Transforma um array de tuplas (estado, probabilidade) em QV
qv :: Basis a => [(a, PA)] -> QV a
qv = fromList

-- Retorna a probabilidade associada a um estado
pr :: Basis a => QV a -> a -> PA
pr qvalue index = Data.Map.findWithDefault 0 index qvalue


-- estado 100% false
qFalse = qv [(False, 1)]

-- estado 100% true
qTrue = qv [(True, 1)]

-- estado 50/50
qVal = qv [(False, 1 / sqrt 2), (True, 1 / sqrt 2) ] 