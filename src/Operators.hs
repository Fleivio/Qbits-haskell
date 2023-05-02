module Operators (hGate, xGate, yGate, zGate) where

import Basis
import Data.Complex

xGate :: QV Bool -> QV Bool 
xGate qval = let a = pr qval False
                 b = pr qval True
             in qv [(False, b), (True, a)]

yGate :: QV Bool -> QV Bool
yGate qval = let a = pr qval False * (0 :+ 1)
                 b = pr qval True  * (0 :+ (-1))
             in qv [(False, b), (True, a)]

zGate :: QV Bool -> QV Bool
zGate qval = let a = pr qval False
                 b = pr qval True * (-1)
             in qv [(False, a), (True, b)]

hGate :: QV Bool -> QV Bool
hGate qval = let a = pr qval False
                 b = pr qval True 
             in  qv [(False, a + b), (True, a - b)]

-- newtype Qop a b = Qop (Map (a,b) PA)

-- qop :: (Basis a, Basis b) => [ ((a,b), PA) ] -> Qop a b 
-- qop = Qop . fromList 