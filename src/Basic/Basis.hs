module Basic.Basis (Basis, basis) where

class (Eq a, Ord a) => Basis a where
  basis :: [a]

-- Base para o tipo bool
instance Basis Bool where
  basis = [False, True]

-- Base para tipos compostos
instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]

-- nao sei se sao necessarias
instance Basis () where
  basis = [()]

-- instance (Basis a, Basis b, Basis c) => Basis (a,b,c)  where
--   basis = [(a,b,c) | a <- basis, b <- basis, c <-basis]
