module Basic.Observation (observeV, observeR, observeLeftR, observeRightR) where

import Basic.QuantumValue
import Basic.Reference
import Basic.Basis
import Basic.Operators
import Basic.ProbabilityAmplitude

import Data.IORef ( readIORef, writeIORef )
import Data.Foldable (find)
import System.Random ( getStdRandom, Random(randomR) )
import Prelude as P
import Data.Complex

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
        writeIORef ptrQval (toQv [(observResult, 1)])
        return observResult

-- Observa o valor da esquerda e colapsa as probabilidades
observeLeftR :: (Basis a, Basis b) => QR (a, b) -> IO a
observeLeftR (QR ptrQval) =
    do 
        qVal <- readIORef ptrQval
        let leftProb a = sqrt . sum $ [ squareModulus (getProb qVal (a,b)) :+ 0| b <- basis]
            leftQval = toQv [(a, leftProb a) | a <- basis]
        observResult <- observeV leftQval
        let nv = toQv [((observResult, b), getProb qVal (observResult, b)) | b <- basis]
        writeIORef ptrQval (normalize nv)
        return observResult

-- Observa o valor da direita e colapsa as probabilidades
observeRightR :: (Basis a, Basis b) => QR (a, b) -> IO b
observeRightR (QR ptrQval) = 
    do 
        qVal <- readIORef ptrQval
        let rightProb a = sqrt . sum $ [ squareModulus (getProb qVal (b,a)) :+ 0| b <- basis]
            rightQval = toQv [(a, rightProb a) | a <- basis]
        observResult <- observeV rightQval
        let nv = toQv [((b, observResult), getProb qVal (b, observResult)) | b <- basis]
        writeIORef ptrQval (normalize nv)
        return observResult