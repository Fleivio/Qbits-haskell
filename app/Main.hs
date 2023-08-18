{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Lambda.Lambda
import BoolOperators
import Virtual.VirtualValue
import Lambda.Constant
import GHC.IO
import Reference.Reference
import Quantum.Value
import Virtual.Adaptor 

{-
 let x = (1,1,0)
 let y = snd (cnot x)
 in read (H y)
-}

virtZ3 :: Virt ((Bool, Bool), Bool) () ((Bool, Bool), Bool)
virtZ3 = virtFromR $ unsafePerformIO (mkQR ( bra_1_ket &* bra_1_ket &* bra_0_ket ))

adaptTrd :: LLT -> LLT
adaptTrd  = LAdaptor (CnstAdaptor (ad_triple3 :: Adaptor (Bool, (Bool, Bool)) ((Bool, Bool), Bool)))

term1 :: LLT
term1 = Let [("x", LValue (CnstValue virtZ3))] $
         Let [("y", adaptTrd (LGate (CnstGate toffoli) `App` Def "x"))] $
         Read (LGate (CnstGate hGate) `App` Def "y")

{-
(\x. 1) (H 0)
-}

virtZ2 :: LLT
virtZ2 =  LValue (CnstValue (virtFromR $ unsafePerformIO (mkQR bra_0_ket)))
virtZ1 :: LLT
virtZ1 =  LValue (CnstValue (virtFromR $ unsafePerformIO (mkQR bra_1_ket)))

term2 :: LLT
term2 = LinAbs virtZ1 `App` (LGate (CnstGate hGate) `App` virtZ2)


main :: IO ()
main =  do
    a <- mkQR (bra_1_ket &* bra_1_ket) 
    b <- mkQR bra_0_ket
    let c = virtFromR a
        d = virtFromR b
        c1 = virtFromV c ad_pair1
    
    comp <- virtTensor c1 d
    app1 cnot comp
    print comp