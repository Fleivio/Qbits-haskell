module Lambda.Const (Const(..)) where

data Const = Zero | One | H | X | Y | Z | Cnot deriving Eq

instance Show Const where 
    show Zero = "q0"
    show One = "q1"
    show H = "H"
    show X = "X"
    show Y = "Y"
    show Z = "Z"
    show Cnot = "Cnot"