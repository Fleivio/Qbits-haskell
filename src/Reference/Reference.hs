module Reference.Reference (mkQR, QR(..), qrApplyOp, qrPrint, qrToString, readIORef, writeIORef) where

import Quantum.Operators
import Quantum.Value

import Data.IORef

-- Referência a um valor quantico (ponteiro)
-- quantum reference
data QR a = QR(IORef (QV a))

-- retorna uma referencia ao valor QR
mkQR :: QV a -> IO(QR a)
mkQR qVal =
    do
        r <- newIORef qVal
        return (QR r)

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