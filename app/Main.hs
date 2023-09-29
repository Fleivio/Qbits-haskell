-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Lambda.Lambda
import BoolOperators
import Lambda.Constant
import Virtual.Adaptor

import Lambda.Interpreter


term3 :: LLT
term3 = ( LGate (CnstGate hGate) `App` LValue cnst1) :&*: LValue cnst0



deutsch1 :: LLT
deutsch1 = Let (("x1", "x2") `match1`
                 (LGate cnstCnot `App`
                    (LGate (CnstGate hGate) `App` LValue cnst1 :&*: LGate (CnstGate hGate) `App` LValue cnst0)))
                (LGate cnstH `App` LAdaptor cnstAdapt1 (Def "x1"))


-- deutsch2 :: LLT 
-- deutsch2 =   Let [("x1", LGate (CnstGate hGate) `App` LValue cnst1),
--                  ("x2", LGate (CnstGate hGate) `App` LValue cnst0)] $
--             NonLazyLet [("x", LGate cnstCnot `App` (Def "x1" :&*: Def "x2"))]
--             (LGate cnstH `App` Def "x1")


main :: IO ResLLT
main = reduction [] deutsch1