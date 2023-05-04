module ProbabilityAmplitude (paToString, PA) where

import Data.Complex
import Text.Printf

type PA = Complex Double 

paToString :: PA -> String
paToString pa 
            | imagPart pa < 0  = printf "(%.1f %.1f⋅i)" (realPart pa) (imagPart pa)
            | imagPart pa > 0  = printf "(%.1f + %.1f⋅i)" (realPart pa) (imagPart pa)
            | otherwise        = printf "%.1f" (realPart pa)