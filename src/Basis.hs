module Basis (qv, pr, Basis, PA, QV, (&*)) where

import Data.Complex
import Data.Map

class (Eq a, Ord a) => Basis a where
    basis :: [a]

-- tipo |Bool>
instance Basis Bool where
    basis = [False, True]

-- tipo |Bool Bool>
instance (Basis a, Basis b) => Basis (a,b) where
    basis = [(a, b) | a <- basis, b <- basis]

-- Probability Amplitude
type PA = Complex Double 

-- Quantum Value
type QV a = Map a PA

-- Transforma um array de tuplas (estado, probabilidade) em QV
qv :: Basis a => [(a, PA)] -> QV a
qv vals = fromList filteredQv
    where filteredQv = Prelude.filter (\(_, pa) -> pa /= 0) vals

-- Retorna a probabilidade associada a um estado
pr :: Basis a => QV a -> a -> PA
pr qvalue index = Data.Map.findWithDefault 0 index qvalue

-- Produto tensorial, <1| &* <0| = <10|
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a,b)
(&*) qa qb = qv [ ( (a,b) , pr qa a * pr qb b) | (a,b) <- basis]


-- estado 100% false |0>
_qFalse :: QV Bool
_qFalse = qv [(False, 1)]

-- estado 100% true |1>
_qTrue :: QV Bool
_qTrue = qv [(True, 1)]

-- estado 50/50
_qVal :: QV Bool
_qVal = qv [ (False, 1 / sqrt 2), (True, 1 / sqrt 2) ]

-- produto tensorial |10>
_qTensor :: QV (Bool, Bool)
_qTensor = _qTrue &* _qFalse

-- estados emaranhados |11> + |00>
_eStates :: QV (Bool, Bool)
_eStates = qv [((False, False), 1), ((True, True), 1)]