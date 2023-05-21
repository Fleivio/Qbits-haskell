module Basic.QuantumValue (getProb, (&*), toQv, braketConvert, QV) where
import Basic.Basis
import Data.Map (Map, findWithDefault, toList, fromList)
import Basic.ProbabilityAmplitude

import Data.List (intercalate)

-- tipo que mapeia um valor da base a uma probabilidade
-- quantum value
type QV a = Map a PA

-- retorna a probabilidade de um valor da base ser medido
getProb :: Basis a => QV a -> a -> PA
getProb qvalue index = Data.Map.findWithDefault 0 index qvalue

-- produto tensorial 
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
(&*) qa qb = toQv [((a, b), getProb qa a * getProb qb b) | (a, b) <- basis]

-- cria um quantum value a partir de uma lista
toQv :: Basis a => [(a, PA)] -> QV a
toQv = fromList . filterZeros
  where
    filterZeros = filter (\(_, pa) -> pa /= 0)

-- converte para uma string
braketConvert :: (Show a) => QV a -> String
braketConvert qvalue = intercalate " + " [paToString pa ++ " ⋅ |" ++ show a ++ "⟩" | (a, pa) <- toList qvalue]
