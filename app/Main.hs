module Main (main) where

import Lambda.Lambda
import Lambda.BasicFunctions

import BoolOperators
import Virtual.VirtualValue


term :: LLT Bool () Bool
term = (LQop hGate) `App` ( LQval virtOne )

main :: IO ()
main =  let a = reductionDebug term
        in do print a
              return ()