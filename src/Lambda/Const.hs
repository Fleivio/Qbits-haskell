module Lambda.Const (Const(..)) where

-- import Quantum.Operators (Qop(..))
-- import Reference.Reference (QR(..))
-- import Quantum.Basis (Basis(..))

data Const = Zero | One | H | CNOT | X | Y | Z | S | T | I deriving (Eq, Show)
