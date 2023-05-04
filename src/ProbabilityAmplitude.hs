module ProbabilityAmplitude (paToString, PA) where

import Data.Complex
import Text.Printf

type PA = Complex Double 

paToString :: PA -> String
paToString pa 
            | imagPart pa < 0  = printf "(%.2f - %.2f)" (realPart pa) (imagPart pa)
            | imagPart pa > 0  = printf "(%.2f + %.2f)" (realPart pa) (imagPart pa)
            | otherwise        = printf "%.2f" (realPart pa)