module BB84.Generator (genBitSet, genBaseSet, prepareQbits) where

import Reference
import System.Random
import BoolOperators
import Operators
import Control.Monad

genBitSet :: Int -> IO [Bool]
genBitSet n = do
  take n . randomRs (True, False) <$> newStdGen

genBaseSet :: Int -> IO [Base]
genBaseSet n = do
  indexes <- take n . randomRs (0, 1) <$> newStdGen
  let values = [X, Z]
  return $ take n [values !! i | i <- indexes]

prepareQbit :: Bool -> Base -> IO(QR Bool)
prepareQbit False Z = mkQR bra_0_ket
prepareQbit True Z = mkQR bra_1_ket
prepareQbit False X = mkQR $ qApp hGate bra_0_ket
prepareQbit True X = mkQR $ qApp hGate bra_1_ket

prepareQbits :: [Bool] -> [Base] -> IO [QR Bool]
prepareQbits = zipWithM prepareQbit