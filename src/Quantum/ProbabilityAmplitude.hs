module Quantum.ProbabilityAmplitude (paToString, PA, squareModulus, addPA) where

import Data.Complex ( imagPart, magnitude, realPart, Complex(..) ) 
import Text.Printf

type PA = Complex Double 

-- Converte uma probabilidade para string
paToString :: PA -> String
paToString pa 
            | imagPart pa < 0  = printf "(%.1f %.1f⋅i)" (realPart pa) (imagPart pa)
            | imagPart pa > 0  = printf "(%.1f + %.1f⋅i)" (realPart pa) (imagPart pa)
            | otherwise        = printf "%.1f" (realPart pa)

-- módulo ao quadrado
squareModulus :: Complex Double -> Double
squareModulus = (**2) . magnitude

addPA :: a -> a -> Complex a
addPA = (:+)