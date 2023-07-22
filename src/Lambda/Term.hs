module Lambda.Term ( Term(..) ) where

class Term a where
    isValue :: a -> Bool
    reductionRun :: a -> a
    reduction :: a -> a
    reductionDebug :: a -> a
    
    shift :: Int -> a -> a
    subst :: Int -> a -> a -> a
    betaReduct :: a -> a -> a
