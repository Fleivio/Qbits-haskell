module Lambda.BasicFunctions (lId, lFalse, lTrue, lIf, lNot, lAnd, lOr, lZero, lOne, lSum,
lSucc, lMult, lPow, lPred, lSub, lIsZro, lEqual, lPair, lLst, lFst, lFix) where

import Lambda.Lambda

bar :: LLT a b c -> LLT a b c
bar = NonLinAbs

-- LOGIC

lId :: LLT a b c
lId = bar (Var 0)

lFalse :: LLT a b c
lFalse = bar (
         bar (
            Var 0))

lTrue :: LLT a b c
lTrue = bar (
        bar (
            Var 1))

lIf :: LLT a b c
lIf = bar (
        bar (
            bar (
                App (App (Var 2) (Var 1)) (Var 0))))

lNot :: LLT a b c
lNot = bar
            ( App (App (App lIf (Var 0) ) lFalse) lTrue )

lAnd :: LLT a b c
lAnd = bar (
            bar (
                App ( App ( App lIf (Var 0) ) (Var 1) ) lFalse
            )
        )

lOr :: LLT a b c
lOr = bar (
        bar (
            App (App ( App lIf (Var 0) ) lTrue) (Var 1)
        )
    )

-- MATHEMATICS

lZero :: LLT a b c
lZero = lFalse


lOne :: LLT a b c
lOne = bar (
        bar (
            App (Var 1) (Var 0)
        )
    )

lSum :: LLT a b c
lSum = bar (
        bar (
            bar (
                bar (
                    App
                    (App (Var 3) (Var 1))
                    (App (App (Var 2) (Var 1)) (Var 0))
                )
            )
        )
    )

lSucc :: LLT a b c
lSucc = bar (
            bar (
                bar (
                    App (Var 1) (App (App (Var 2) (Var 1)) (Var 0))
                )
            )
    )


lMult :: LLT a b c
lMult = bar (
        bar (
            bar (
                App (Var 2) (App (Var 1) (Var 0))
            )
        )
    )

lPow :: LLT a b c
lPow = bar (
        bar (
            App (Var 1) (Var 0)
        )
    )

_zz :: LLT a b c
_zz = App (App lPair lZero) lZero

_ss :: LLT a b c
_ss = bar ( App
                (App lPair (App lLst (Var 0)))
                (App lSucc (App lLst (Var 0) ))
        )

lPred :: LLT a b c
lPred = bar (
            App lFst (App (App (Var 0) _ss) _zz)
    )

lSub :: LLT a b c
lSub = bar (
        bar (
            App (App (Var 0) lPred) (Var 1)
        )
    )

lIsZro :: LLT a b c
lIsZro = bar (
        App (App (Var 0) (bar lFalse)) lTrue
    )


lEqual :: LLT a b c
lEqual =
    bar (
        bar (
            App (App lAnd
                (App lIsZro (App (App lSub (Var 0)) (Var 1))))
                (App lIsZro (App (App lSub (Var 1)) (Var 0)))
        )
    )

-- PAIRS

lPair :: LLT a b c
lPair = bar (
        bar (
            bar (
                App (App (Var 0) (Var 2)) (Var 1)
            )
        )
    )

lLst :: LLT a b c
lLst = bar (
        App (Var 0) lFalse
    )

lFst :: LLT a b c
lFst = bar (
        App (Var 0) lTrue
    )

-- RECURSIVE 
lFix :: LLT a b c
lFix = bar ( -- !u     1    
            bar  -- ! f 0
            (
                Var 0 `App` NonLinTerm ( (Var 1 `App` NonLinTerm (Var 1)) `App` NonLinTerm (Var 0) )
            )
        )
        `App` 
        NonLinTerm (bar ( -- !u     1    
            bar  -- ! f 0
            (
                Var 0 `App` NonLinTerm ( (Var 1 `App` NonLinTerm (Var 1)) `App` NonLinTerm (Var 0) )
            )
        ))

