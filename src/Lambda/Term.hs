{-# LANGUAGE DefaultSignatures #-}
module Lambda.Term ( Term(..) ) where

import qualified Debug.Trace as Debug

class (Eq a, Show a) => Term a where
    isValue :: a -> Bool
    reductionRun :: a -> a
    reduction :: a -> a
    reductionDebug :: a -> a
    shift :: Int -> a -> a
    subst :: Int -> a -> a -> a
    betaReduct :: a -> a -> a

    default reductionDebug :: a -> a
    reductionDebug t =
        let t' = reductionRun t
        in  if t' == t
            then t
            else Debug.trace (show t ++ " --> " ++ show t') (reductionDebug t')
    
    default reduction :: a -> a 
    reduction t =
        let t' = reductionRun t
        in  if t' == t
            then t
            else reduction t'



