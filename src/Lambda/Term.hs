module Lambda.Term ( Term(..) ) where

class Term a where
    isValue :: a -> Bool
    reduction :: a -> a
    reductionPrint :: a -> IO a
    
    shift :: Int -> a -> a
    subst :: Int -> a -> a -> a
    betaReduct :: a -> a -> a
