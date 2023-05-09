module QuantumValue (getProb, (&*), toQv, braketConvert, QV) where
import Basis
import Data.Map (Map, findWithDefault, toList, fromList)
import ProbabilityAmplitude

import Data.List (intercalate)

type QV a = Map a PA

getProb :: Basis a => QV a -> a -> PA
getProb qvalue index = Data.Map.findWithDefault 0 index qvalue

(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
(&*) qa qb = toQv [((a, b), getProb qa a * getProb qb b) | (a, b) <- basis]

toQv :: Basis a => [(a, PA)] -> QV a
toQv = fromList . filterZeros
  where
    filterZeros = filter (\(_, pa) -> pa /= 0)

braketConvert :: (Show a) => QV a -> String
braketConvert qvalue = intercalate " + " [paToString pa ++ " ⋅ |" ++ show a ++ "⟩" | (a, pa) <- toList qvalue]
