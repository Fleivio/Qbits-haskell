{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Lambda.Constant(QGate(..), QValue(..), gateApply, readValue) where

import Data.Typeable


import Quantum.Basis
import Virtual.VirtualValue
import Quantum.Operators


data QGate = forall a b. (Basis a, Basis b) => QGate (Qop a b) 
data QValue = forall a b c. (Basis a, Basis b, Basis c) => QValue (Virt a b c)

instance Show QGate where
    show (QGate (op :: Qop a1 a2)) = case eqT @a1 @a2 of
      Just Refl -> show op
      _ -> error "need an `Op a a`, not a general `Op a b`" 


instance Show QValue where
    show (QValue (v :: Virt a b c)) = show v

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
