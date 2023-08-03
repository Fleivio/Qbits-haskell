module Lambda.VarTable(VarName, VarTable, lookUpVar, varAppend) where
import Debug.Trace
type VarName = String

type VarTable a = [(VarName, a)]


lookUpVar :: VarTable a -> VarName -> Maybe a 
lookUpVar [] n = trace ("Nada de " ++ show n) Nothing
lookUpVar ((name, val):xs) name' 
    | name == name' = Just val
    | otherwise = lookUpVar xs name'

varAppend :: VarTable a -> VarTable a -> VarTable a
varAppend v1 v2 = v1 ++ v2