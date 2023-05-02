module Basis (toQv, prob, Basis, PA, QV, (&*)) where

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
toQv :: Basis a => [(a, PA)] -> QV a
toQv vals = fromList filteredQv
    where filteredQv = Prelude.filter (\(_, pa) -> pa /= 0) vals

-- Retorna a probabilidade associada a um estado
prob :: Basis a => QV a -> a -> PA
prob qvalue index = Data.Map.findWithDefault 0 index qvalue

-- Produto tensorial, <1| &* <0| = <10|
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a,b)
(&*) qa qb = toQv [ ( (a,b) , prob qa a * prob qb b) | (a,b) <- basis]


-- estado 100% false |0>
_qFalse :: QV Bool
_qFalse = toQv [(False, 1)]

-- estado 100% true |1>
_qTrue :: QV Bool
_qTrue = toQv [(True, 1)]

-- estado 50/50
_qVal :: QV Bool
_qVal = toQv [ (False, 1 / sqrt 2), (True, 1 / sqrt 2) ]

-- produto tensorial |10>
_qTensor :: QV (Bool, Bool)
_qTensor = _qTrue &* _qFalse

-- estados emaranhados |11> + |00>
_eStates :: QV (Bool, Bool)
_eStates = toQv [((False, False), 1), ((True, True), 1)]