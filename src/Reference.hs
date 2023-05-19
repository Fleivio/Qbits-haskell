module Reference (mkQR, observe, observeRight, observeLeft, QR, qrApplyOp, observeAtBasis, ObsBasis(..), printRef, refToString) where

import QuantumValue ( QV, getProb, toQv, braketConvert)
import Data.Map (singleton)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Operators
import BoolOperators
import Basis ( Basis(..) )
import Data.Complex ( Complex((:+)) )

import ProbabilityAmplitude (squareModulus)

data QR a = QR(IORef (QV a))
data ObsBasis = X | Z deriving (Show, Eq)

-- retorna uma referencia ao valor QR
mkQR :: QV a -> IO(QR a)
mkQR qVal =
    do
        r <- newIORef qVal
        return (QR r)

-- observa o valor de QR e colapsa a probabilidade para 100% no valor resultante
observe :: Basis a => QR a -> IO a
observe (QR ptrQval) =
    do
        qVal <- readIORef ptrQval
        observResult <- observeV qVal
        writeIORef ptrQval (singleton observResult 1)
        return observResult

-- Observa o valor da esquerda e colapsa as probabilidades
observeLeft :: (Basis a, Basis b) => QR (a, b) -> IO a
observeLeft (QR ptrQval) =
    do 
        qVal <- readIORef ptrQval
        let leftProb a = sqrt . sum $ [ squareModulus (getProb qVal (a,b)) :+ 0| b <- basis]
            leftQval = toQv [(a, leftProb a) | a <- basis]
        observResult <- observeV leftQval
        let nv = toQv [((observResult, b), getProb qVal (observResult, b)) | b <- basis]
        writeIORef ptrQval (normalize nv)
        return observResult

-- Observa o valor da direita e colapsa as probabilidades
observeRight :: (Basis a, Basis b) => QR (a, b) -> IO b
observeRight (QR ptrQval) = 
    do 
        qVal <- readIORef ptrQval
        let rightProb a = sqrt . sum $ [ squareModulus (getProb qVal (b,a)) :+ 0| b <- basis]
            rightQval = toQv [(a, rightProb a) | a <- basis]
        observResult <- observeV rightQval
        let nv = toQv [((b, observResult), getProb qVal (b, observResult)) | b <- basis]
        writeIORef ptrQval (normalize nv)
        return observResult

qrApplyOp :: (Basis a) => Qop a a -> QR a -> IO()
qrApplyOp op (QR ptrQval) =
    do
        qVal <- readIORef ptrQval
        let tqv = qApp op qVal
        writeIORef ptrQval tqv

observeAtBasis :: ObsBasis -> QR Bool -> IO Bool
observeAtBasis Z qr = 
    do
        qrApplyOp idGate qr
        observe qr
observeAtBasis X qr = 
    do
        qrApplyOp hGate qr
        a <- observe qr
        qrApplyOp hGate qr
        return a

refToString ::(Show a) => QR a -> IO String 
refToString (QR ioref) = do
    qval <- readIORef ioref 
    return $ braketConvert qval

printRef :: (Show a) => QR a -> IO()
printRef (QR ioref) = do
    qval <- readIORef ioref 
    putStrLn $ braketConvert qval