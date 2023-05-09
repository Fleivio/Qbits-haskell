module ProbabilityAmplitude (paToString, PA, squareModulus) where

import Data.Complex
import Text.Printf

type PA = Complex Double 

paToString :: PA -> String
paToString pa 
            | imagPart pa < 0  = printf "(%.1f %.1f⋅i)" (realPart pa) (imagPart pa)
            | imagPart pa > 0  = printf "(%.1f + %.1f⋅i)" (realPart pa) (imagPart pa)
            | otherwise        = printf "%.1f" (realPart pa)

squareModulus :: Complex Double -> Double
squareModulus a = (**2) $ magnitude a