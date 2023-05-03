module Operators (hGate, xGate, yGate, zGate) where

import QuantumValue
import Data.Complex

xGate :: QV Bool -> QV Bool 
xGate qval = let a = getProb qval False
                 b = getProb qval True
             in toQv [(False, b), (True, a)]

yGate :: QV Bool -> QV Bool
yGate qval = let a = getProb qval False * (0 :+ 1)
                 b = getProb qval True  * (0 :+ (-1))
             in toQv [(False, b), (True, a)]

zGate :: QV Bool -> QV Bool
zGate qval = let a = getProb qval False
                 b = getProb qval True * (-1)
             in toQv [(False, a), (True, b)]

hGate :: QV Bool -> QV Bool
hGate qval = let a = getProb qval False
                 b = getProb qval True 
             in  toQv [(False, a + b), (True, a - b)]