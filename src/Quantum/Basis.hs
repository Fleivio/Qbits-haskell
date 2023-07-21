module Quantum.Basis (Basis, basis) where

class (Eq a, Ord a) => Basis a where
  basis :: [a]

-- Base para o tipo bool
instance Basis Bool where
  basis = [False, True]

-- Base para tipos compostos
instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]

instance Basis () where
  basis = [()]