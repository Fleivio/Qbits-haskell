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

extraId = NonLinAbs (lId `App` NonLinTerm (LQval ref0))

term = (LQop hGate `App`  ( LQval ref0 ))


main :: IO ()
main =  let a = reductionDebug term
        in do print a
              putStrLn "----------------"
              return ()