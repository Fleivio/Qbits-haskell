module QuantumValue (getProb, (&*), toQv, braketConvert, QV) where

import Basis (Basis (..))
import Data.Map (Map, findWithDefault, fromList, toList)
import ProbabilityAmplitude (PA, paToString)

type QV a = Map a PA

-- Probabilidade associada a um estado
getProb :: Basis a => QV a -> a -> PA
getProb qvalue index = Data.Map.findWithDefault 0 index qvalue

-- Produto tensorial
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
(&*) qa qb = toQv [((a, b), getProb qa a * getProb qb b) | (a, b) <- basis]

-- Transforma um array de tuplas (estado, probabilidade) em QV
toQv :: Basis a => [(a, PA)] -> QV a
toQv vals = fromList filteredQv
  where
    filteredQv = Prelude.filter (\(_, pa) -> pa /= 0) vals

-- Converte para notação bra-ket
braketConvert :: (Show a) => QV a -> String
braketConvert qvalue = foldl1 (++) [paToString pa ++ "*|" ++ show a ++ "⟩" | (a, pa) <- toList qvalue]

-- EXEMPLOS

-- estado 100% false |0>
_qFalse :: QV Bool
_qFalse = toQv [(False, 1)]

-- estado 100% true |1>
_qTrue :: QV Bool
_qTrue = toQv [(True, 1)]

-- estado 50/50
_qVal :: QV Bool
_qVal = toQv [(False, 1 / sqrt 2), (True, 1 / sqrt 2)]

-- produto tensorial |10>
_qTensor :: QV (Bool, Bool)
_qTensor = _qTrue &* _qFalse

-- estados emaranhados |11> + |00>
_eStates :: QV (Bool, Bool)
_eStates = toQv [((False, False), 1), ((True, True), 1)]