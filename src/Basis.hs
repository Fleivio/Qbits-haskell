module Basis (Basis, basis) where

class (Eq a, Ord a) => Basis a where
    basis :: [a]

-- base do espaço vetorial |Bool>
instance Basis Bool where
    basis = [False, True]

-- base |Bool Bool>
instance (Basis a, Basis b) => Basis (a,b) where
    basis = [(a, b) | a <- basis, b <- basis]

instance (Basis a) => Basis [a] where
    basis = [ x:xs | x <- basis, xs <- tail basis]
