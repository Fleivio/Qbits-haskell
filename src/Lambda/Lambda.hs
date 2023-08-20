module Lambda.Lambda (LLT (..), betaReduct, isValue, defToLLT) where

import Lambda.Constant
import Lambda.VarTable

data LLT = -- Classic
    Var Int
  | NonLinAbs LLT
  | NonLinTerm LLT
  | App LLT LLT
   -- Linear
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
  LAdaptor _ t1 == LAdaptor _ t2 = t1 == t2
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

betaReduct :: LLT -> LLT -> LLT
betaReduct s t = shift (-1) (subst 0 (shift 0 s) t)

