{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lambda.Lambda (LLT (..), reduction, reductionDebug) where

import GHC.IO
import Lambda.Constant
import Lambda.VarTable

data LLT = -- Classic
    Var Int
  | NonLinAbs LLT
  | NonLinTerm LLT
  | App LLT LLT
   -- Lineaar
  | LinAbs LLT
   -- Variables
  | Def VarName 
  | Let (VarTable LLT) LLT
   -- Quantum
  | LValue CnstValue
  | LAdaptor CnstAdaptor LLT
  | LGate CnstGate
  | LLT :&*: LLT
  | Read LLT

instance Eq LLT where
  Var i == Var j = i == j
  LinAbs t1 == LinAbs t2 = t1 == t2
  NonLinAbs t1 == NonLinAbs t2 = t1 == t2
  NonLinTerm t1 == NonLinTerm t2 = t1 == t2
  App t1 t2 == App t3 t4 = t1 == t3 && t2 == t4
  Read t1 == Read t2 = t1 == t2
  LGate g1 == LGate g2 = g1 == g2
  LValue v1 == LValue v2 = v1 == v2
  LAdaptor c1 t1 == LAdaptor c2 t2 = t1 == t2
  Def n1 == Def n2 = n1 == n2
  Let v1 n1 == Let v2 n2 = v1 == v2 && n1 == n2
  a :&*: b == a1 :&*: b1 = a == b && a1 == b1
  _ == _ = False

instance Show LLT where
  show (Var i) = show i
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (NonLinAbs t) = "λ!" ++ show t
  show (LinAbs t) = "λ" ++ show t
  show (NonLinTerm t) = "!(" ++ show t ++ ")"
  show (LGate q) = show q
  show (LValue v) = show v
  show (LAdaptor _ v) = "adapt " ++ show v
  show (Read t) = "read[" ++ show t ++ "]"
  show (Def name) = name
  show (Let vt t) = "let " ++ show vt ++ " in \n" ++ show t
  show (a :&*: b) = "(" ++ show a ++ ", " ++ show b ++ ")"

isValue :: LLT -> Bool
isValue (App _ _) = False
isValue _ = True

defToLLT :: VarTable LLT -> VarName -> LLT
defToLLT vt name = maybe (Def name) id (lookUpVar vt name) 

reductionRun :: VarTable LLT -> LLT -> LLT
-- app
reductionRun vt (App t1 t2)
  | not (isValue t1) = App (reductionRun vt t1) t2 
  | not (isValue t2) = App t1 (reductionRun vt t2)
-- beta
reductionRun _ (App (LinAbs t) v)
  | isValue v =
      betaReduct v t
-- !beta!
reductionRun _ (App (NonLinAbs t) (NonLinTerm v)) =
  betaReduct v t
-- qop
reductionRun _ (App (LGate q) (LValue v)) =
  unsafePerformIO
    ( do
        _ <- cnstApp q v
        return $ LValue v                             
    )
-- read
reductionRun _ (Read (LValue v)) =
  unsafePerformIO
    ( do
        _ <- cnstRead v
        return $ LValue v                            
    )
-- tensor
reductionRun _ ((LValue v1) :&*: (LValue v2)) =
  unsafePerformIO
    ( do
        v3 <- cnstTensor v1 v2
        return $ LValue v3
    )
-- adapt
reductionRun vt (LAdaptor ad (LValue v)) =
  reductionRun vt $ LValue $ cnstAdapt v ad
-- base cases
reductionRun vt (App (LGate q) t) = App (LGate q) (reductionRun vt t)
reductionRun vt (LAdaptor ad t) = LAdaptor ad (reductionRun vt t)
reductionRun vt (Read t) = Read (reductionRun vt t)
reductionRun vt (Def name) = defToLLT vt name
reductionRun vt (Let vt' in') = reductionDebug (varAppend vt vt') in'
reductionRun vt (v1 :&*: v2) = reductionRun vt v1 :&*: reductionRun vt v2
reductionRun _ a = a

shift :: Int -> LLT -> LLT
shift d = walk 0
  where
    walk c t = case t of
      Var x ->
        if x >= c
          then Var (x + d)
          else Var x
      NonLinAbs t1 -> NonLinAbs (walk (c + 1) t1)
      LinAbs t1 -> LinAbs (walk (c + 1) t1)
      App t1 t2 -> App (walk c t1) (walk c t2)
      NonLinTerm t1 -> NonLinTerm (walk c t1)
      t1 -> t1

subst :: Int -> LLT -> LLT -> LLT
subst j s = walk 0
  where
    walk c t = case t of
      Var x ->
        if x == j + c
          then shift (c + 1) s
          else Var x
      NonLinAbs t1 -> NonLinAbs (walk (c + 1) t1)
      LinAbs t1 -> LinAbs (walk (c + 1) t1)
      App t1 t2 -> App (walk c t1) (walk c t2)
      NonLinTerm t1 -> NonLinTerm (walk c t1)
      t1 -> t1

reductionDebug :: VarTable LLT -> LLT -> LLT
reductionDebug vt t =
  let t' =
        unsafePerformIO
          ( do
              putStr (show t ++ " >>> ")
              let reduct = reductionRun vt t
              print reduct
              return reduct
          )
  in if t' == t
        then t'
        else reductionDebug vt t'

reduction :: VarTable LLT -> LLT -> LLT
reduction vt t =
  let t' = reductionRun vt t
  in if t' == t
        then t'
        else reduction vt t'

betaReduct :: LLT -> LLT -> LLT
betaReduct s t = shift (-1) (subst 0 (shift 0 s) t)

