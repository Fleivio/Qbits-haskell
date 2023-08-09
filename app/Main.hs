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

{-
 let x = (1,1,0)
 let y = snd (cnot x)
 in read (H y)
-}


adaptTrd :: LLT -> LLT
adaptTrd  = LAdaptor (QAdaptor (ad_triple3 :: Adaptor (Bool, (Bool, Bool)) ((Bool, Bool), Bool)))

term1 :: LLT
term1 = Let [("x", LValue (QValue (virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket )))))] $
         Let [("y", adaptTrd (LGate (QGate toffoli) `App` (Def "x")))] $
         Read (LGate (QGate hGate) `App` (Def "y"))

virtZ2 =  LValue (QValue (virtFromR $ unsafePerformIO (mkQR bra_0_ket)))
virtZ1 =  LValue (QValue (virtFromR $ unsafePerformIO (mkQR bra_1_ket)))

term2 :: LLT
term2 = (LinAbs virtZ1) `App` (LGate (QGate hGate) `App` virtZ2)


main :: IO ()
main =  let c = reductionDebug [] term1
            d = reductionDebug [] term2
        in do print c
              putStrLn "----------------"
              print d
              return ()