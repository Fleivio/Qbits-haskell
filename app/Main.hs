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
adaptTrd  = LAdaptor (QAdaptor (ad_triple3 :: Adaptor (Bool, (Bool, Bool)) ((Bool, Bool), Bool)))

term1 :: LLT
term1 = Let [("x", LValue (QValue virtZ3))] $
         Let [("y", adaptTrd (LGate (QGate toffoli) `App` Def "x"))] $
         Read (LGate (QGate hGate) `App` Def "y")

{-
(\x. 1) (H 0)
-}

virtZ2 :: LLT
virtZ2 =  LValue (QValue (virtFromR $ unsafePerformIO (mkQR bra_0_ket)))
virtZ1 :: LLT
virtZ1 =  LValue (QValue (virtFromR $ unsafePerformIO (mkQR bra_1_ket)))

term2 :: LLT
term2 = LinAbs virtZ1 `App` (LGate (QGate hGate) `App` virtZ2)


main :: IO ()
main =  do
    a <- mkQR (bra_0_ket &* bra_1_ket) 
    b <- mkQR (bra_0_ket)

    c <- qrTensor a b

    print c

    let k = bra_0_ket &* bra_1_ket &* bra_0_ket 
    print $ k
    