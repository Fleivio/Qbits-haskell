{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lambda.Lambda (Term(..), LLT(..), adaptLLT) where

import Lambda.Term
import Quantum.Operators (Qop(..))
import Virtual.VirtualValue
import Quantum.Basis
import GHC.IO
import Virtual.Adaptor

data LLT a b c = 
      Var Int
    | LinAbs (LLT a b c)
    | NonLinAbs (LLT a b c)
    | NonLinTerm (LLT a b c) 
    | App (LLT a b c) (LLT a b c)
    | LGate (Qop a a)
    | LValue (Virt a b c)
    | Read (LLT a b c)

adaptLLT :: (Basis a, Basis na, Basis u) => LLT a na u -> Adaptor (a1, a2) a -> LLT a1 (a2, na) u
adaptLLT (LValue v) a = LValue (virtFromV v a)
adaptLLT v a = case reductionDebug v of
    (LinAbs v1) -> LinAbs (adaptLLT v1 a)
    (NonLinAbs v1) -> NonLinAbs (adaptLLT v1 a)
    (NonLinTerm v1) -> NonLinTerm (adaptLLT v1 a)
    (App v1 v2) -> App (adaptLLT v1 a) (adaptLLT v2 a)
    (Var i) -> Var i
    LValue v1 -> adaptLLT (LValue v1) a

instance (Basis a, Basis b, Basis c) => Eq (LLT a b c) where
    Var i == Var j = i == j
    LinAbs t1 == LinAbs t2 = t1 == t2
    NonLinAbs t1 == NonLinAbs t2 = t1 == t2
    NonLinTerm t1 == NonLinTerm t2 = t1 == t2
    App t1 t2 == App t3 t4 = t1 == t3 && t2 == t4
    Read t1 == Read t2 = t1 == t2
    LGate _ == LGate _ = True
    LValue _ == LValue _ = True
    _ == _ = False

instance (Basis a, Basis b, Basis c) => Show (LLT a b c) where 
    show (Var i) = show i
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (NonLinAbs t) = "λ!" ++ show t
    show (LinAbs t)    = "λ" ++ show t
    show (NonLinTerm t) = "!(" ++ show t ++ ")"
    show (LGate q) = show q 
    show (LValue v) = show v
    show (Read t) = "read[" ++ show t ++ "]"

instance (Basis a, Basis b, Basis c) => Term (LLT a b c) where
    isValue (App _ _) = False
    isValue _ = True

    reductionRun (App t1 t2) 
        | not (isValue t1)       = App (reductionRun t1) t2  -- app 1    
        | not (isValue t2)       = App t1 (reductionRun t2)  -- app 2
    reductionRun (App (LinAbs t) v) 
        | isValue v              = betaReduct v t            -- beta
    reductionRun (App (NonLinAbs t) (NonLinTerm v)) 
                                 = betaReduct v t            -- !beta
    reductionRun (App (LGate q) (LValue v))
        = unsafePerformIO (
            do  _ <- app1 q v
                return $ LValue v                             -- qop 
            )
    reductionRun (Read (LValue v))
        = unsafePerformIO (
            do  _ <- observeVV v
                return $ LValue v                             -- read
            )

    -- base cases
    reductionRun (Read t) = Read (reductionRun t)            
    reductionRun a = a                                       

    shift d = walk 0
        where walk c t = case t of
                    Var x -> if x >= c
                            then Var ( x + d )
                            else Var x
                    NonLinAbs t1 -> NonLinAbs (walk (c+1) t1)
                    LinAbs t1 -> LinAbs (walk (c+1) t1)
                    App t1 t2 -> App (walk c t1) (walk c t2)
                    NonLinTerm t1 -> NonLinTerm (walk c t1)
                    t1 -> t1

    subst j s = walk 0
        where walk c t = case t of
                    Var x -> if x == j + c
                            then shift (c + 1) s
                            else Var x
                    NonLinAbs t1 -> NonLinAbs (walk (c + 1) t1)
                    LinAbs t1 -> LinAbs (walk (c + 1) t1)
                    App t1 t2 -> App (walk c t1) (walk c t2)
                    NonLinTerm t1 -> NonLinTerm (walk c t1)
                    t1 -> t1

    betaReduct s t = shift (-1) (subst 0 (shift 0 s) t)