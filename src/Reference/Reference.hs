module Reference.Reference (mkQR, QR(..), (&&*), qrTensor, qrApplyOp, qrPrint, qrToString, readIORef, writeIORef) where

import Quantum.Operators
import Quantum.Value

import Data.IORef
import GHC.IO (unsafePerformIO)

-- Referência a um valor quantico (ponteiro)
-- quantum reference
newtype QR a = QR(IORef (QV a))

instance (Show a) => Show (QR a) where
    show (QR ioref) = unsafePerformIO $ qrToString (QR ioref)

-- retorna uma referencia ao valor QR
mkQR :: QV a -> IO(QR a)
mkQR qVal =
    do
        r <- newIORef qVal
        return (QR r)

(&&*) :: (Basis a, Basis b) => QR a -> QR b -> IO (QR (a, b))
(&&*) = qrTensor

qrTensor :: (Basis a, Basis b) => QR a -> QR b -> IO (QR (a, b))
qrTensor (QR ptrQval1) (QR ptrQval2) =
    do
        qVal1 <- readIORef ptrQval1
        qVal2 <- readIORef ptrQval2
        let tqv = qVal1 &* qVal2
        mkQR tqv

-- aplica uma operação a uma referência, atualiza o resultado
qrApplyOp :: (Basis a) => Qop a a -> QR a -> IO ()
qrApplyOp op (QR ptrQval) =
    do
        qVal <- readIORef ptrQval
        let tqv = qApp op qVal
        writeIORef ptrQval tqv

qrToString :: (Show a) => QR a -> IO String 
qrToString (QR ioref) = do
    qval <- readIORef ioref 
    return $ qvToString qval

qrPrint :: (Show a) => QR a -> IO()
qrPrint (QR ioref) = do
    qval <- readIORef ioref 
    putStrLn $ qvToString qval