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

adaptSnd :: LLT -> LLT
adaptSnd  = LAdaptor (QAdaptor (ad_triple2 :: Adaptor (Bool, (Bool, Bool)) ((Bool, Bool), Bool)))


term1 :: LLT
term1 = Let [("x", LValue (QValue (virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket )))))] $
         Let [("y", adaptSnd (LGate (QGate toffoli) `App` (Def "x")))] 
         (LGate (QGate hGate) `App` (Def "y"))

{-
 let x = (H 1, 1)
 in cnot x
-}


main :: IO ()
main =  let c = reductionDebug [] term1 
        in do print c
              putStrLn "----------------"
              return ()