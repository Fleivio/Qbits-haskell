module Quantum.Value (QV, Basis(..), PA, getProb, (&*), mkQV, qvToString, qvTensor, squareModulus, addPA) where

import Quantum.Basis
import Quantum.ProbabilityAmplitude

import Data.Map as Map (Map, fromList, toList, findWithDefault)
import Data.List (intercalate)

-- tipo que mapeia um valor da base a uma probabilidade
type QV a = Map a PA

-- retorna a probabilidade de um valor da base ser medido
getProb :: Basis a => QV a -> a -> PA
getProb qvalue index = Map.findWithDefault 0 index qvalue

-- produto tensorial 
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
(&*) qa qb = mkQV [((a, b), getProb qa a * getProb qb b) | (a, b) <- basis]

qvTensor :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
qvTensor = (&*)

-- cria um quantum value a partir de uma lista
mkQV :: Basis a => [(a, PA)] -> QV a
mkQV = fromList . filterZeros
  where
    filterZeros = filter (\(_, pa) -> pa /= 0)

-- converte para uma string
qvToString :: (Show a) => QV a -> String
qvToString qvalue = intercalate " + " [paString pa ++ "|" ++ show a ++ "⟩" | (a, pa) <- toList qvalue, pa /= 0]
  where paString p = if p == 1 then "" else paToString p ++ "·"
  
