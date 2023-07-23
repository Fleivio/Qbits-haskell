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
    | LQop (Qop a a)
    | LQval (Virt a b c)

adaptLLT :: (Basis a, Basis na, Basis u) => LLT a na u -> Adaptor (a1, a2) a -> LLT a1 (a2, na) u
adaptLLT (LQval v) a = LQval (virtFromV v a)
adaptLLT a b = adaptLLT (reductionDebug a) b

instance (Basis a, Basis b, Basis c) => Eq (LLT a b c) where
    Var i == Var j = i == j
    LinAbs t1 == LinAbs t2 = t1 == t2
    NonLinAbs t1 == NonLinAbs t2 = t1 == t2
    NonLinTerm t1 == NonLinTerm t2 = t1 == t2
    App t1 t2 == App t3 t4 = t1 == t3 && t2 == t4
    LQop _ == LQop _ = True
    LQval _ == LQval _ = True
    _ == _ = False

instance (Basis a, Basis b, Basis c) => Show (LLT a b c) where 
    show (Var i) = show i
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (NonLinAbs t) = "λ!" ++ show t
    show (LinAbs t)    = "λ" ++ show t
    show (NonLinTerm t) = "!(" ++ show t ++ ")"
    show (LQop q) = show q 
    show (LQval v) = show v

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
    reductionRun (App (LQop q) (LQval v))
         = unsafePerformIO (
            do  _ <- app1 q v
                return $ LQval v                             -- qop 
            ) 
    reductionRun a = a                                       -- id

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