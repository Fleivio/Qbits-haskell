{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Lambda.Interpreter(reduction, ResLLT) where

import Lambda.Lambda
import Lambda.VarTable
import Lambda.Constant

import Control.Concurrent

import Lambda.ResultLog

type ResLLT = ResultLog LLT

reductionRun :: VarTable LLT -> LLT -> IO LLT
reductionRun vt t = case t of
    (App t1 t2) | not (isValue t1)  -> reductionRun vt t1 >>= \x -> return $ App x t2
                | not (isValue t2)  -> reductionRun vt t2 >>= \x -> return $ App t1 x
    (App (LinAbs t1) v) | isValue v -> return $ betaReduct v t1
    (App (NonLinAbs t1) (NonLinTerm v)) | isValue v -> return $ betaReduct v t1
    (App (LGate q) (LValue v)) -> do
        _ <- cnstApp q v
        return $ LValue v
    (Read (LValue v)) -> do
        _ <- cnstRead v
        return $ LValue v
    ((LValue v1) :&*: (LValue v2)) -> do
        v3 <- cnstTensor v1 v2
        return $ LValue v3
    (LAdaptor ad (LValue v)) -> reductionRun vt $ LValue $ cnstAdapt v ad
    (App (LGate q) t1)       ->  reductionRun vt t1 >>= return . App (LGate q)
    (LAdaptor ad t1) -> reductionRun vt t1 >>= return . LAdaptor ad
    (Read t1)        -> fmap Read (reductionRun vt t1)
    (Def name)       -> return $ defToLLT vt name
    (Let vt' in')    -> reduction (varAppend vt vt') in' >>= return . res
    (v1 :&*: v2)     -> do
        a <- reductionRun vt v1
        b <- reductionRun vt v2
        return $ a :&*: b
    a -> return a

reduction :: VarTable LLT -> LLT -> IO ResLLT
reduction vt t = do
    threadDelay 10000
    t' <- reductionRun vt t
    let
        log' = show t ++ " >>> " ++ show t' ++ "\n"
        result = Res t' log'
    if t' == t
        then return result
        else (\r -> do
                r1 <- reduction vt (res r)
                return $ concatRes r r1 
             ) result