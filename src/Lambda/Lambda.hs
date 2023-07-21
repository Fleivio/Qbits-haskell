module Lambda.Lambda (Term(..), Const(..), LLT(..)) where
import Lambda.Const
import Lambda.Term

data LLT = 
      Var Int
    | Const Const
    | LinAbs LLT
    | NonLinAbs LLT
    | NonLinTerm LLT 
    | App LLT LLT
    deriving Eq

instance Show LLT where 
    show (Var i) = show i
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (NonLinAbs t) = "/!" ++ show t
    show (LinAbs t)    = "/" ++ show t
    show (Const c)     = show c
    show (NonLinTerm t) = "!(" ++ show t ++ ")"


instance Term LLT where
    isValue (App _ _) = False
    isValue _ = True

    reduction (App t1 t2) 
        | not (isValue t1) && not (isValue t2) = App (reduction t1) t2  -- app 1    
        | isValue t1 && not (isValue t2)       = App t1 (reduction t2)  -- app 2
    reduction (App (LinAbs t) v) 
        | isValue v                            = betaReduct v t         -- beta
    reduction (App (NonLinAbs t) (NonLinTerm v)) 
        = betaReduct v t                                                -- !b1
        -- b2 nao tratado
    reduction a = a

    reductionPrint t = do
        let t' = reduction t
        if t' == t
            then return t
            else do
                putStrLn $ show t ++ " -> " ++ show t'
                reductionPrint t'

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