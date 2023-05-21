module Basic.Reference (mkQR, QR(..), qrApplyOp, printRef, refToString) where

import Basic.Operators ( Qop, qApp )
import Basic.Basis ( Basis(..) )
import Basic.QuantumValue ( QV, braketConvert )

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

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
qrApplyOp :: (Basis a) => Qop a a -> QR a -> IO()
qrApplyOp op (QR ptrQval) =
    do
        qVal <- readIORef ptrQval
        let tqv = qApp op qVal
        writeIORef ptrQval tqv

refToString ::(Show a) => QR a -> IO String 
refToString (QR ioref) = do
    qval <- readIORef ioref 
    return $ braketConvert qval

printRef :: (Show a) => QR a -> IO()
printRef (QR ioref) = do
    qval <- readIORef ioref 
    putStrLn $ braketConvert qval