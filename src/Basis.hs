module Basis (qv, pr, qVal) where

import Data.Complex
import Data.Map

class (Eq a, Ord a) => Basis a where
    basis :: [a]

instance Basis Bool where
    basis = [False, True]


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

instance (Basis a, Basis b) => Basis (a,b) where
    basis = [(a, b) | a <- basis, b <- basis]

-- Produto tensorial
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a,b)
(&*) qa qb = qv [ ( (a,b) , pr qa a * pr qb b) | (a,b) <- basis] 

-- estados emaranhados
eStates = qv [((False, False), 1), ((True, True), 1)]