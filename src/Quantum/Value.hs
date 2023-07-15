module Quantum.Value (getProb, (&*), mkQV, qvToString, QV, Basis(..), PA, squareModulus, addPA) where

import Quantum.Basis
import Quantum.ProbabilityAmplitude

import Data.Map as Map (Map, fromList, toList, findWithDefault)
import Data.List (intercalate)

-- tipo que mapeia um valor da base a uma probabilidade
-- quantum value
type QV a = Map a PA

-- retorna a probabilidade de um valor da base ser medido
getProb :: Basis a => QV a -> a -> PA
getProb qvalue index = Map.findWithDefault 0 index qvalue

-- produto tensorial 
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
(&*) qa qb = mkQV [((a, b), getProb qa a * getProb qb b) | (a, b) <- basis]

-- cria um quantum value a partir de uma lista
mkQV :: Basis a => [(a, PA)] -> QV a
mkQV = fromList . filterZeros
  where
    filterZeros = filter (\(_, pa) -> pa /= 0)

-- converte para uma string
qvToString :: (Show a) => QV a -> String
qvToString qvalue = intercalate " + " [paToString pa ++ " ⋅ |" ++ show a ++ "⟩" | (a, pa) <- toList qvalue]
