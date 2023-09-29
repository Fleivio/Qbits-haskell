-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Lambda.Lambda
import BoolOperators
import Lambda.Constant
import Virtual.Adaptor

import Lambda.Interpreter
import Lambda.Interpreter (ResLLT)


term3 :: LLT
term3 = ( LGate (CnstGate hGate) `App` LValue cnst1) :&*: LValue cnst0



-- deutsch1 :: LLT
-- deutsch1 = Let (("x1", "x2") `match1`
--                  (LGate cnstCnot `App`
--                     (LGate (CnstGate hGate) `App` LValue cnst1 :&*: LGate (CnstGate hGate) `App` LValue cnst0)))
--                 (LGate cnstH `App` LAdaptor cnstAdapt1 (Def "x1"))

adapt1 :: CnstAdaptor
adapt1 = CnstAdaptor (ad_pair1 :: Adaptor (Bool, Bool) (Bool, Bool)) 

deutch1 :: LLT 
deutch1 = Let [("x", LGate cnstCnot `App` ((LGate (CnstGate hGate) `App` LValue cnst1) :&*: (LGate (CnstGate hGate) `App` LValue cnst0)))]
                (LGate cnstH `App` LAdaptor adapt1 (Def "x"))

main :: IO ResLLT
main = 
    do 
        a <- reduction [] deutch1 
        print a
        return undefined