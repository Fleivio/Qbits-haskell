{-# LANGUAGE DefaultSignatures #-}
module Lambda.Term ( Term(..) ) where

import GHC.IO

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
        let t' = unsafePerformIO (
                do putStr (show t ++ " >>> " )
                   let reduct = reductionRun t
                   print reduct
                   return reduct
                )
        in  if t' == t
            then t
            else reductionDebug t'
                
                
    default reduction :: a -> a 
    reduction t =
        let t' = reductionRun t
        in  if t' == t
            then t
            else reduction t'



