{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main (main) where

import Basis

ket_01_bra :: QV (Bool, Bool)
ket_01_bra = toQv [(False, 1)] &* toQv [(True, 1)]

main :: IO ()
main = print ket_01_bra