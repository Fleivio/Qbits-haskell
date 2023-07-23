module Quantum.Operators (qop, qApp, cqop, normalize, Qop(..)) where

import Quantum.Value
    ( PA, squareModulus, Basis(..), QV, getProb, mkQV, addPA )
import Data.Map as Map (Map, fromList, toList, map)
import Prelude as P

-- Operador, mapeia um valor a para um valor b, dada uma probabilidade
data Qop a b = Qop (Map (a, b) PA) String deriving Eq

instance Show (Qop a b) where
    show (Qop _ name) = show name

-- cria um operador a partir de um vetor
qop :: (Basis a, Basis b) => [((a,b), PA)] -> String -> Qop a b
qop mp = Qop (fromList mp)

-- cria um operador controlado, recebe uma funcao que controla se a operação será ou não feita
cqop :: (Basis a, Basis b) => (a -> Bool) -> Qop b b -> String -> Qop (a, b) (a, b)
cqop enable (Qop u _) = qop ( unchangeCase ++ changeCase )
    where
        unchangeCase = [( ((a, b) , (a, b)), 1 ) | (a, b) <- basis, not (enable a)] -- enable = false
        changeCase = [( ((a, b1) , (a, b2)), getProb u (b1, b2) ) | a <- basis, enable a, b1 <- basis, b2 <- basis] -- enable = true

-- aplica uma operação a um valor quantico, retorna o resultado
qApp :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
qApp (Qop mp _) qval = mkQV [ (b, probB b) | b <- basis ]
    where
        probB b = sum [ probToMap (a, b) * probOriginal a | a <- basis ]
        probOriginal = getProb qval
        probToMap = getProb mp

-- norma do QV
norm :: QV a -> Double
norm v = sqrt . sum $ probs
    where probs = P.map (squareModulus . snd) (toList v)

-- normaliza as probabilidades
normalize :: QV a -> QV a
normalize qval = Map.map (c*) qval
    where
        c = 1 / norm qval `addPA` 0

