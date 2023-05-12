module Basis (Basis, basis) where

class (Eq a, Ord a) => Basis a where
  basis :: [a]

instance Basis Bool where
  basis = [False, True]

instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]
