module Main (main) where

-- import BB84.BB84
-- 
-- main :: IO()
-- main = bb84interfRun

import Basic.QuantumValue
import Basic.Reference
import BoolOperators
import VirtualValues.Adaptor
import VirtualValues.VirtualValue

main :: IO()
main = do 
  qr <- mkQR ( toQv [((True, True, True), 1), ((False, True, False), 1)] )
  let vr = virtFromR qr
  toffoli' vr
  printRef qr