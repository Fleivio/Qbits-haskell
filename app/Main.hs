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


adaptT1 :: LLT -> LLT
adaptT1  = LAdaptor (QAdaptor (ad_triple1 :: Adaptor (Bool, (Bool, Bool)) ((Bool, Bool), Bool)))

term3 :: LLT
term3 = LGate (QGate hGate) `App` (adaptT1 (LValue value))
      where value = QValue (virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket )))

-- let x = 1,0,1,1 tipo total
--     y = adaptor_13 x
--     cnot y 

main :: IO ()
main =  let c = reductionDebug term3
        in do print c
              putStrLn "----------------"
              return ()