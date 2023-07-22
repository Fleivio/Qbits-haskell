module Lambda.BasicFunctions (lId, lFalse, lTrue, lIf, lNot, lAnd, lOr, lZero, lOne, lSum,
lSucc, lMult, lPow, lPred, lSub, lIsZro, lEqual, lPair, lLst, lFst) where

import Lambda.Lambda

bar :: LLT -> LLT
bar = NonLinAbs

-- LOGIC

lId :: LLT
lId = bar (Var 0)

lFalse :: LLT
lFalse = bar (
         bar (
            Var 0))

lTrue :: LLT
lTrue = bar (
        bar (
            Var 1))

lIf :: LLT
lIf = bar (
        bar (
            bar (
                App (App (Var 2) (Var 1)) (Var 0))))

lNot :: LLT
lNot = bar
            ( App (App (App lIf (Var 0) ) lFalse) lTrue )

lAnd :: LLT
lAnd = bar (
            bar (
                App ( App ( App lIf (Var 0) ) (Var 1) ) lFalse
            )
        )

lOr :: LLT
lOr = bar (
        bar (
            App (App ( App lIf (Var 0) ) lTrue) (Var 1)
        )
    )

-- MATHEMATICS

lZero :: LLT
lZero = lFalse


lOne :: LLT
lOne = bar (
        bar (
            App (Var 1) (Var 0)
        )
    )

lSum :: LLT
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

lSucc :: LLT
lSucc = bar (
            bar (
                bar (
                    App (Var 1) (App (App (Var 2) (Var 1)) (Var 0))
                )
            )
    )


lMult :: LLT
lMult = bar (
        bar (
            bar (
                App (Var 2) (App (Var 1) (Var 0))
            )
        )
    )

lPow :: LLT
lPow = bar (
        bar (
            App (Var 1) (Var 0)
        )
    )

_zz :: LLT
_zz = App (App lPair lZero) lZero

_ss :: LLT
_ss = bar ( App
                (App lPair (App lLst (Var 0)))
                (App lSucc (App lLst (Var 0) ))
        )

lPred :: LLT
lPred = bar (
            App lFst (App (App (Var 0) _ss) _zz)
    )

lSub :: LLT
lSub = bar (
        bar (
            App (App (Var 0) lPred) (Var 1)
        )
    )

lIsZro :: LLT
lIsZro = bar (
        App (App (Var 0) (bar lFalse)) lTrue
    )


lEqual :: LLT
lEqual =
    bar (
        bar (
            App (App lAnd
                (App lIsZro (App (App lSub (Var 0)) (Var 1))))
                (App lIsZro (App (App lSub (Var 1)) (Var 0)))
        )
    )

-- PAIRS

lPair :: LLT
lPair = bar (
        bar (
            bar (
                App (App (Var 0) (Var 2)) (Var 1) 
            )
        )
    )

lLst :: LLT
lLst = bar (
        App (Var 0) lFalse
    )

lFst :: LLT
lFst = bar (
        App (Var 0) lTrue
    )
