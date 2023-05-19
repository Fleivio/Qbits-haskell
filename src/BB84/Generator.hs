module BB84.Generator (genBitSet, genBasisSet, prepareQbits) where

import Basic.Reference
import System.Random
import BoolOperators
import Basic.Operators
import Control.Monad

genBitSet :: Int -> IO [Bool]
genBitSet n = do
  take n . randomRs (True, False) <$> newStdGen

genBasisSet :: Int -> IO [ObsBasis]
genBasisSet n = do
  indexes <- take n . randomRs (0, 1) <$> newStdGen
  let values = [X, Z]
  return $ take n [values !! i | i <- indexes]

prepareQbit :: Bool -> ObsBasis -> IO(QR Bool)
prepareQbit False Z = mkQR bra_0_ket
prepareQbit True Z = mkQR bra_1_ket
prepareQbit False X = mkQR $ qApp hGate bra_0_ket
prepareQbit True X = mkQR $ qApp hGate bra_1_ket

prepareQbits :: [Bool] -> [ObsBasis] -> IO [QR Bool]
prepareQbits = zipWithM prepareQbit