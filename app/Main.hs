module Main (main) where

import Lambda.Lambda
import Lambda.BasicFunctions

import BoolOperators
import Virtual.VirtualValue


term :: LLT Bool () Bool
term = (LQop xGate) `App` ( LQval virtOne )
-- term = lId `App` NonLinTerm ( Const B )

main :: IO ()
main =  let a = reductionDebug term
        in do print a
              return ()