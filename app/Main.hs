-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Lambda.Lambda
import BoolOperators
import Lambda.Constant
import Virtual.Adaptor

import Lambda.Interpreter


adapt1 :: CnstAdaptor
adapt1 = CnstAdaptor (ad_pair1 :: Adaptor (Bool, Bool) (Bool, Bool)) 

deutch1 :: LLT 
deutch1 = Let [("x", LGate cnstCnot `App` ((LGate (CnstGate hGate) `App` LValue cnst1) :&*: (LGate (CnstGate hGate) `App` LValue cnst0)))]
                (LGate cnstH `App` LAdaptor adapt1 (Def "x"))

deutsch2 :: LLT 
deutsch2 =
    Let [("x1", LGate (CnstGate hGate) `App` LValue cnst1),
         ("x2", LGate (CnstGate hGate) `App` LValue cnst0)] $
    Let [("x", LGate cnstCnot `App` (Def "x1" :&*: Def "x2"))] $
          LGate cnstH `App` Def "x1"

main :: IO ()
main = 
    do 
        a <- reduction [] deutch1 
        print a