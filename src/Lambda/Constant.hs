{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Lambda.Constant(CnstGate(..), CnstValue(..), CnstAdaptor(..), gateApply, readValue, adaptValue) where

import Data.Typeable


import Quantum.Basis
import Virtual.VirtualValue
import Quantum.Operators
import Virtual.Adaptor

data CnstGate = forall a b. (Basis a, Basis b) => CnstGate (Qop a b)
data CnstValue = forall a b c. (Basis a, Basis b, Basis c) => CnstValue (Virt a b c)
data CnstAdaptor = forall a b c. (Basis a, Basis b, Basis c) => CnstAdaptor (Adaptor (a, b) c)

instance Show CnstGate where
    show (CnstGate op) = show op

instance Eq CnstGate where
    (CnstGate (op1 :: Qop a1 a2)) == (CnstGate (op2 :: Qop a3 a4)) 
        = case eqT @a1 @a3 of
            Just Refl -> case eqT @a2 @a4 of
                Just Refl -> op1 == op2
                _ -> False
            _ -> False

instance Show CnstValue where
    show (CnstValue v) = show v

instance Eq CnstValue where
    (CnstValue (_ :: Virt a1 b1 c1)) == (CnstValue (_ :: Virt a2 b2 c2)) 
        = case eqT @a1 @a2 of
            Just Refl -> case eqT @b1 @b2 of
                Just Refl -> case eqT @c1 @c2 of
                    Just Refl -> True
                    _ -> False
                _ -> False
            _ -> False

instance Show CnstAdaptor where
    show _ = "adaptor"

adaptValue :: CnstValue -> CnstAdaptor -> CnstValue
adaptValue (CnstValue (v :: Virt a rest u)) (CnstAdaptor (ad :: Adaptor (f1, f2) t)) 
    = case eqT @a @t of
        Just Refl -> CnstValue (virtFromV v ad)
        _ -> error "Cound't match adaptor with value basis"

gateApply :: CnstGate -> CnstValue -> IO()
gateApply (CnstGate (op :: Qop a1 a2)) (CnstValue (v :: Virt a3 b c)) 
    = case eqT @a1 @a2 of                   -- is transitioning from a1 to a1
      Just Refl -> case eqT @a1 @a3 of      -- basis of operation match basis of value
        Just Refl -> app1 op v
        _ -> error "`Op a a` must match `Val a b c`"
      _ -> error "need an `Op a a`, not a general `Op a b`"

readValue :: CnstValue -> IO ()
readValue (CnstValue (v :: Virt a1 b c)) 
    = do 
    _ <- observeVV v
    return ()

