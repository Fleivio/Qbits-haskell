module Main (main) where

import Lambda.Lambda
import Lambda.BasicFunctions

import BoolOperators
import Virtual.VirtualValue
import GHC.IO
import Reference.Reference
import Quantum.Value
import Lambda.Lambda
import Virtual.Adaptor

term2 = adaptLLT (LGate toffoli `App` LValue (virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket )))) ad_triple1

term :: LLT Bool () Bool
term = Read $ LGate xGate `App` (LGate hGate `App` ( NonLinAbs (lId `App` NonLinTerm (Var 0))  
                                    `App` NonLinTerm ( LValue virtOne )))





main :: IO ()
main =  let a = reductionDebug term
            b = reductionDebug term2
        in do print a
              putStrLn "----------------"
              print b
              return ()