module Main (main) where

import BoolOperators

main :: IO()
main =
    do
        adder bra_1_ket bra_1_ket bra_1_ket
        putStrLn "---"
        adder bra_1_ket bra_1_ket bra_0_ket
        putStrLn "---"
        adder bra_1_ket bra_0_ket bra_0_ket