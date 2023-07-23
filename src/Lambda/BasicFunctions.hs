module Lambda.BasicFunctions (lId, lFalse, lTrue, lIf, lNot, lAnd, lOr, lZero, lOne, lSum,
lSucc, lMult, lPow, lPred, lSub, lIsZro, lEqual, lPair, lLst, lFst, lFix) where

import Lambda.Lambda

bar :: LLT a -> LLT a
bar = NonLinAbs

-- LOGIC

lId :: LLT a
lId = bar (Var 0)

lFalse :: LLT a
lFalse = bar (
         bar (
            Var 0))

lTrue :: LLT a
lTrue = bar (
        bar (
            Var 1))

lIf :: LLT a
lIf = bar (
        bar (
            bar (
                App (App (Var 2) (Var 1)) (Var 0))))

lNot :: LLT a
lNot = bar
            ( App (App (App lIf (Var 0) ) lFalse) lTrue )

lAnd :: LLT a
lAnd = bar (
            bar (
                App ( App ( App lIf (Var 0) ) (Var 1) ) lFalse
            )
        )

lOr :: LLT a
lOr = bar (
        bar (
            App (App ( App lIf (Var 0) ) lTrue) (Var 1)
        )
    )

-- MATHEMATICS

lZero :: LLT a
lZero = lFalse


lOne :: LLT a
lOne = bar (
        bar (
            App (Var 1) (Var 0)
        )
    )

lSum :: LLT a
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

lSucc :: LLT a
lSucc = bar (
            bar (
                bar (
                    App (Var 1) (App (App (Var 2) (Var 1)) (Var 0))
                )
            )
    )


lMult :: LLT a
lMult = bar (
        bar (
            bar (
                App (Var 2) (App (Var 1) (Var 0))
            )
        )
    )

lPow :: LLT a
lPow = bar (
        bar (
            App (Var 1) (Var 0)
        )
    )

_zz :: LLT a
_zz = App (App lPair lZero) lZero

_ss :: LLT a
_ss = bar ( App
                (App lPair (App lLst (Var 0)))
                (App lSucc (App lLst (Var 0) ))
        )

lPred :: LLT a
lPred = bar (
            App lFst (App (App (Var 0) _ss) _zz)
    )

lSub :: LLT a
lSub = bar (
        bar (
            App (App (Var 0) lPred) (Var 1)
        )
    )

lIsZro :: LLT a
lIsZro = bar (
        App (App (Var 0) (bar lFalse)) lTrue
    )


lEqual :: LLT a
lEqual =
    bar (
        bar (
            App (App lAnd
                (App lIsZro (App (App lSub (Var 0)) (Var 1))))
                (App lIsZro (App (App lSub (Var 1)) (Var 0)))
        )
    )

-- PAIRS

lPair :: LLT a
lPair = bar (
        bar (
            bar (
                App (App (Var 0) (Var 2)) (Var 1)
            )
        )
    )

lLst :: LLT a
lLst = bar (
        App (Var 0) lFalse
    )

lFst :: LLT a
lFst = bar (
        App (Var 0) lTrue
    )

-- RECURSIVE 
lFix :: LLT a
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

