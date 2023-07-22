{-# LANGUAGE DefaultSignatures #-}
module Lambda.Term ( Term(..) ) where

import qualified Debug.Trace as Debug

class Term a where
    isValue :: a -> Bool
    reductionRun :: a -> a
    reduction :: a -> a
    reductionDebug :: a -> a

    default reductionDebug :: (Show a, Eq a) => a -> a
    reductionDebug t =
        let t' = reductionRun t
        in  if t' == t
            then t
            else Debug.trace (show t ++ " --> " ++ show t') (reductionDebug t')
    
    default reduction :: (Eq a) => a -> a 
    reduction t =
        let t' = reductionRun t
        in  if t' == t
            then t
            else reduction t'


    shift :: Int -> a -> a
    subst :: Int -> a -> a -> a
    betaReduct :: a -> a -> a
