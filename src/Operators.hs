module Operators (hGate, xGate, yGate, zGate) where

import Basis
import Data.Complex

xGate :: QV Bool -> QV Bool 
xGate qval = let a = prob qval False
                 b = prob qval True
             in toQv [(False, b), (True, a)]

yGate :: QV Bool -> QV Bool
yGate qval = let a = prob qval False * (0 :+ 1)
                 b = prob qval True  * (0 :+ (-1))
             in toQv [(False, b), (True, a)]

zGate :: QV Bool -> QV Bool
zGate qval = let a = prob qval False
                 b = prob qval True * (-1)
             in toQv [(False, a), (True, b)]

hGate :: QV Bool -> QV Bool
hGate qval = let a = prob qval False
                 b = prob qval True 
             in  toQv [(False, a + b), (True, a - b)]

-- newtype Qop a b = Qop (Map (a,b) PA)

-- qop :: (Basis a, Basis b) => [ ((a,b), PA) ] -> Qop a b 
-- qop = Qop . fromList 