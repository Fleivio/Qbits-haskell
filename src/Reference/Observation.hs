{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Reference.Observation (observeV, observeR, observeLeftR, observeRightR) where

import Quantum.Value
import Quantum.Operators
import Reference.Reference

import Data.Foldable (find)
import System.Random ( getStdRandom, Random(randomR) )
import Prelude as P

-- observa um valor quatnico, retorna o resultado
observeV :: Basis a => QV a -> IO a
observeV v =
    do
        let nv = normalize v
            probs = P.map (squareModulus . getProb nv) basis
        r <- getStdRandom (randomR (0.0, 1.0))
        let cPsCs = zip (scanl1 (+) probs) basis
            Just (_, res) = find (\(p, _) -> r < p) cPsCs
        return res


-- observa o valor de QR e colapsa a probabilidade para 100% no valor resultante
observeR :: Basis a => QR a -> IO a
observeR (QR ptrQval) =
    do
        qVal <- readIORef ptrQval
        observResult <- observeV qVal
        writeIORef ptrQval (mkQV [(observResult, 1)])
        return observResult

-- Observa o valor da esquerda e colapsa as probabilidades
observeLeftR :: (Basis a, Basis b) => QR (a, b) -> IO a
observeLeftR (QR ptrQval) =
    do 
        qVal <- readIORef ptrQval
        let leftProb a = sqrt . sum $ [ squareModulus (getProb qVal (a,b)) `addPA` 0| b <- basis]
            leftQval = mkQV [(a, leftProb a) | a <- basis]
        observResult <- observeV leftQval
        let nv = mkQV [((observResult, b), getProb qVal (observResult, b)) | b <- basis]
        writeIORef ptrQval (normalize nv)
        return observResult

-- Observa o valor da direita e colapsa as probabilidades
observeRightR :: (Basis a, Basis b) => QR (a, b) -> IO b
observeRightR (QR ptrQval) = 
    do 
        qVal <- readIORef ptrQval
        let rightProb a = sqrt . sum $ [ squareModulus (getProb qVal (b,a)) `addPA` 0| b <- basis]
            rightQval = mkQV [(a, rightProb a) | a <- basis]
        observResult <- observeV rightQval
        let nv = mkQV [((b, observResult), getProb qVal (b, observResult)) | b <- basis]
        writeIORef ptrQval (normalize nv)
        return observResult