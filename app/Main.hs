module Main (main) where

import Lambda.Lambda
import Lambda.BasicFunctions

import BoolOperators
import Virtual.VirtualValue
import Lambda.Constant
import GHC.IO
import Reference.Reference
import Quantum.Value
import Lambda.Lambda
import Virtual.Adaptor

-- term2 = adaptLLT (LGate toffoli `App` LValue (virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket )))) ad_triple1

term = Read $ LGate (QGate xGate) `App` (LGate (QGate hGate) `App` ( NonLinAbs (lId `App` NonLinTerm (Var 0))  
                                    `App` NonLinTerm ( LValue (QValue virtOne) )))


-- term3 = Read $ LGate hGate `App` (adaptLLT (LValue (virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket )))) ad_triple1)


main :: IO ()
main =  let c = reductionDebug term
        in do print c
              putStrLn "----------------"
              return ()