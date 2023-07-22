module Main (main) where
{-# LANGUAGE BangPatterns #-}

import Lambda.Lambda
import Lambda.BasicFunctions

-- y :: IO (QR Bool)
-- y = mkQR $ mkQV [(True, 1)]

-- x :: IO (QR Bool)
-- x = mkQR $ mkQV [(True, 1)]

-- cin :: IO (QR Bool)
-- cin = mkQR $ mkQV [(True, 1)]

-- cout :: IO (QR Bool)
-- cout = mkQR $ mkQV [(False, 1)]

-- sres :: IO (QR Bool)
-- sres = mkQR $ mkQV [(False, 1)]

-- main :: IO()
-- main =
--     do
--         xqr <- x
--         yqr <- y
--         cinqr <- cin
--         coutqr <- cout
--         sresqr <- sres
--         _ <- adderRef xqr yqr cinqr coutqr sresqr
--         putStrLn "----references----"
--         putStrLn "----carry----"
--         qrPrint coutqr
--         putStrLn "----result----"
--         qrPrint sresqr


main :: IO ()
main =  let a = reductionDebug $ NonLinAbs (Const Zero) `App` NonLinTerm (App (Const H) (Const Zero))
            b = reductionDebug $ App (App (NonLinAbs (NonLinAbs (App (Var 1) (Var 0)))) (NonLinTerm lId)) (NonLinTerm(NonLinTerm(Const Zero))) 
            fix = reductionDebug $ App lFix $ NonLinAbs (Const Zero `App` Var 0)
        in do print a
              putStrLn "\n----\n"
              print b
              putStrLn "\n----\n"
              print fix
              return ()

{-
(fix !(t)) --> (λ!(0 !((fix !(0)))) !(t))
(λ!(0 !((fix !(0)))) !(t)) --> (t !((fix !(t))))
(t !((fix !(t))))

(fix λ!(q0 0)) --> (λ!(0 !((fix !(0)))) λ!(q0 0))
(λ!(0 !((fix !(0)))) λ!(q0 0))

-}