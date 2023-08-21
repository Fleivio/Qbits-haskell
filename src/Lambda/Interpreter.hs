module Lambda.Interpreter(reduction) where

import Lambda.Lambda
import Lambda.VarTable
import GHC.IO
import Lambda.Constant

import Lambda.ResultLog

type ResLLT = ResultLog LLT

reductionRun :: VarTable LLT -> LLT -> LLT
reductionRun vt t = case t of
    -- app
    (App t1 t2) | not (isValue t1) -> App (reductionRun vt t1) t2
                | not (isValue t2) -> App t1 (reductionRun vt t2)
    -- beta
    (App (LinAbs t1) v) | isValue v -> betaReduct v t1
    -- !beta!
    (App (NonLinAbs t1) (NonLinTerm v)) | isValue v -> betaReduct v t1
    -- qop
    (App (LGate q) (LValue v)) ->
        unsafePerformIO
            ( do
                _ <- cnstApp q v
                return $ LValue v                             
            )
    -- read
    (Read (LValue v)) ->
        unsafePerformIO
            ( do
                _ <- cnstRead v
                return $ LValue v                            
            )
    -- tensor
    ((LValue v1) :&*: (LValue v2)) ->
        unsafePerformIO
            ( do
                v3 <- cnstTensor v1 v2
                return $ LValue v3
            )
    -- adapt
    (LAdaptor ad (LValue v)) ->
        reductionRun vt $ LValue $ cnstAdapt v ad
    -- base cases
    (App (LGate q) t1) -> App (LGate q) (reductionRun vt t1)
    (LAdaptor ad t1) -> LAdaptor ad (reductionRun vt t1)
    (Read t1)        -> Read (reductionRun vt t1)
    (Def name)      -> defToLLT vt name
    (Let vt' in')   -> res $ reduction (varAppend vt vt') in'
    (v1 :&*: v2)    -> reductionRun vt v1 :&*: reductionRun vt v2
    a -> a

reduction :: VarTable LLT -> LLT -> ResLLT
reduction vt t =
  let t' = reductionRun vt t
      log' = show t ++ " >>> " ++ show t' ++ "\n"
      result = Res t' log'
  in if t' == t
        then result
        else result >>= reduction vt