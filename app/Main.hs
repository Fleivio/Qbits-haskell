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

term2 :: LLT Bool (Bool, ()) (Bool, Bool)
term2 = adaptLLT (LQop cnot `App` LQval (virtFromR $ unsafePerformIO (mkQR (mkQV [((False, True), 1)]) ))) ad_pair1

term :: LLT Bool () Bool
term = LQop xGate `App` (LQop hGate `App` ( NonLinAbs (lId `App` NonLinTerm (Var 0))  
                                    `App` NonLinTerm ( LQval virtOne )))


main :: IO ()
main =  let a = reductionDebug term
            b = reductionDebug term2
        in do print a
              putStrLn "----------------"
              print b
              return ()