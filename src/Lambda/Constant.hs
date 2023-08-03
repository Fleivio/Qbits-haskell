{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Lambda.Constant(QGate(..), QValue(..), QAdaptor(..), gateApply, readValue, adaptValue) where

import Data.Typeable


import Quantum.Basis
import Virtual.VirtualValue
import Quantum.Operators
import Virtual.Adaptor

data QGate = forall a b. (Basis a, Basis b) => QGate (Qop a b)
data QValue = forall a b c. (Basis a, Basis b, Basis c) => QValue (Virt a b c)
data QAdaptor = forall a b c. (Basis a, Basis b, Basis c) => QAdaptor (Adaptor (a, b) c)

instance Show QGate where
    show (QGate op) = show op

instance Eq QGate where
    (QGate (op1 :: Qop a1 a2)) == (QGate (op2 :: Qop a3 a4)) 
        = case eqT @a1 @a3 of
            Just Refl -> case eqT @a2 @a4 of
                Just Refl -> op1 == op2
                _ -> False
            _ -> False

instance Show QValue where
    show (QValue v) = show v

instance Eq QValue where
    (QValue (_ :: Virt a1 b1 c1)) == (QValue (_ :: Virt a2 b2 c2)) 
        = case eqT @a1 @a2 of
            Just Refl -> case eqT @b1 @b2 of
                Just Refl -> case eqT @c1 @c2 of
                    Just Refl -> True
                    _ -> False
                _ -> False
            _ -> False

instance Show QAdaptor where
    show _ = "adaptor"

adaptValue :: QValue -> QAdaptor -> QValue
adaptValue (QValue (v :: Virt a rest u)) (QAdaptor (ad :: Adaptor (f1, f2) t)) 
    = case eqT @a @t of
        Just Refl -> QValue (virtFromV v ad)
        _ -> error "Cound't match adaptor with value basis"

gateApply :: QGate -> QValue -> IO()
gateApply (QGate (op :: Qop a1 a2)) (QValue (v :: Virt a3 b c)) 
    = case eqT @a1 @a2 of                   -- is transitioning from a1 to a1
      Just Refl -> case eqT @a1 @a3 of      -- basis of operation match basis of value
        Just Refl -> app1 op v
        _ -> error "`Op a a` must match `Val a b c`"
      _ -> error "need an `Op a a`, not a general `Op a b`"

readValue :: QValue -> IO ()
readValue (QValue (v :: Virt a1 b c)) 
    = do 
    _ <- observeVV v
    return ()

