module Main (main) where

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
main = do
    _ <- reductionPrint $ NonLinAbs (Const Zero) `App` NonLinTerm (App (Const H) (Const Zero))
    putStrLn "\n"
    _ <- reductionPrint $ App (App (NonLinAbs (NonLinAbs (App (Var 1) (Var 0)))) (NonLinTerm lId)) (NonLinTerm(NonLinTerm(Const Zero))) 
    putStrLn "\n"