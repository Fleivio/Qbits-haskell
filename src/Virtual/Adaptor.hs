{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Virtual.Adaptor (Adaptor(..), ad_triple13, ad_triple23, ad_triple12, ad_triple3, ad_triple2, ad_triple1, ad_triple, ad_pair2, ad_pair1, ad_quad1, ad_quad2, ad_quad3, ad_quad4, ad_quad12, ad_quad13, ad_quad23, ad_quad123, ad_quad124, ad_quad134, ad_quad234, ad_quad, ad_quad34) where

-- Funcões que removem e colocam um valor quantico dentro de seu contexto
-- Muito útil para isolarmos um valor de restante da estrutura

-- ua -> o contexto total original
-- ta -> tupla (valor, resto)

data Adaptor ta ua = Adaptor {dec :: ua -> ta, cmp :: ta -> ua}

ad_pair1 :: Adaptor (a1, a2) (a1, a2)
ad_pair1 = Adaptor {
    dec = \(a1, a2) -> (a1, a2),
    cmp = \(a1, a2) -> (a1, a2)
}

ad_pair2 :: Adaptor (a2, a1) (a1, a2)
ad_pair2 = Adaptor {
    dec = \(a1, a2) -> (a2, a1),
    cmp = \(a2, a1) -> (a1, a2)
}

ad_triple1 :: Adaptor (a1, (a2, a3)) ((a1, a2), a3)
ad_triple1 = Adaptor {
    dec = \((a1, a2), a3) -> (a1, (a2, a3)),
    cmp = \(a1, (a2, a3)) -> ((a1, a2), a3) 
}

ad_triple2 :: Adaptor (a2, (a1, a3)) ((a1, a2), a3)
ad_triple2 = Adaptor {
    dec = \((a1, a2), a3) -> (a2, (a1, a3)),
    cmp = \(a2, (a1, a3)) -> ((a1, a2), a3)
}

ad_triple3 :: Adaptor (a3, (a1, a2)) ((a1, a2), a3) 
ad_triple3 = Adaptor {
    dec = \((a1, a2), a3) -> (a3, (a1, a2)),
    cmp = \(a3, (a1, a2)) -> ((a1, a2), a3)
}

ad_triple12 :: Adaptor ((a1, a2), a3) ((a1, a2), a3)
ad_triple12 = Adaptor {
    dec = \((a1, a2), a3) -> ((a1, a2), a3),
    cmp = \((a1, a2), a3) -> ((a1, a2), a3) 
}

ad_triple13 :: Adaptor ((a1, a3), a2) ((a1, a2), a3)
ad_triple13 = Adaptor {
    dec = \((a1, a2), a3) -> ((a1, a3), a2),
    cmp = \((a1, a3), a2) -> ((a1, a2), a3) 
}

ad_triple23 :: Adaptor ((a2, a3), a1) ((a1, a2), a3)
ad_triple23 = Adaptor {
    dec = \((a1, a2), a3) -> ((a2, a3), a1),
    cmp = \((a2, a3), a1) -> ((a1, a2), a3) 
}

ad_triple :: Adaptor (((a1,a2),a3), ()) ((a1,a2),a3)
ad_triple = Adaptor {
    dec = \((a1, a2), a3) -> (((a1,a2),a3), ()),
    cmp = \(((a1, a2), a3), ()) -> ((a1,a2),a3)
}

ad_quad1 :: Adaptor (a1, ((a2, a3), a4)) (((a1, a2), a3), a4)
ad_quad1 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (a1, ((a2, a3), a4)),
    cmp = \(a1, ((a2, a3), a4)) -> (((a1, a2), a3), a4)
}

ad_quad2 :: Adaptor (a2, (a1, a3, a4)) (((a1, a2), a3), a4)
ad_quad2 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (a2, (a1, a3, a4)),
    cmp = \(a2, (a1, a3, a4)) -> (((a1, a2), a3), a4)
}

ad_quad3 :: Adaptor (a3, (a1, a2, a4)) (((a1, a2), a3), a4)
ad_quad3 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (a3, (a1, a2, a4)),
    cmp = \(a3, (a1, a2, a4)) -> (((a1, a2), a3), a4)
}

ad_quad4 :: Adaptor (a4, (a1, a2, a3)) (((a1, a2), a3), a4)
ad_quad4 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (a4, (a1, a2, a3)),
    cmp = \(a4, (a1, a2, a3)) -> (((a1, a2), a3), a4)
}

ad_quad12 :: Adaptor ((a1, a2), (a3, a4)) (((a1, a2), a3), a4)
ad_quad12 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> ((a1, a2), (a3, a4)),
    cmp = \((a1, a2), (a3, a4)) -> (((a1, a2), a3), a4)
}

ad_quad13 :: Adaptor ((a1, a3), (a2, a4)) (((a1, a2), a3), a4)
ad_quad13 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> ((a1, a3), (a2, a4)),
    cmp = \((a1, a3), (a2, a4)) -> (((a1, a2), a3), a4)
}

ad_quad23 :: Adaptor ((a2, a3), (a1, a4)) (((a1, a2), a3), a4)
ad_quad23 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> ((a2, a3), (a1, a4)),
    cmp = \((a2, a3), (a1, a4)) -> (((a1, a2), a3), a4)
}

ad_quad34 :: Adaptor ((a3, a4), (a1, a2)) (((a1,a2),a3),a4)
ad_quad34 = Adaptor {
    dec = \(((a1,a2),a3),a4) -> ((a3, a4), (a1, a2)),
    cmp = \((a3, a4), (a1, a2)) -> (((a1,a2),a3),a4)
}

ad_quad123 :: Adaptor (((a1, a2), a3), a4) (((a1, a2), a3), a4)
ad_quad123 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (((a1, a2), a3), a4),
    cmp = \(((a1, a2), a3), a4) -> (((a1, a2), a3), a4)
}

ad_quad124 :: Adaptor (((a1, a2), a4), a3) (((a1, a2), a3), a4)
ad_quad124 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (((a1, a2), a4), a3),
    cmp = \(((a1, a2), a4), a3) -> (((a1, a2), a3), a4)
}

ad_quad134 :: Adaptor (((a1, a3), a4), a2) (((a1, a2), a3), a4)
ad_quad134 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (((a1, a3), a4), a2),
    cmp = \(((a1, a3), a4), a2) -> (((a1, a2), a3), a4)
}

ad_quad234 :: Adaptor (((a2, a3), a4), a1) (((a1, a2), a3), a4)
ad_quad234 = Adaptor {
    dec = \(((a1, a2), a3), a4) -> (((a2, a3), a4), a1),
    cmp = \(((a2, a3), a4), a1) -> (((a1, a2), a3), a4)
}

ad_quad :: Adaptor ((((a1,a2),a3),a4), ()) (((a1,a2),a3),a4)
ad_quad = Adaptor {
    dec = \(((a1,a2),a3),a4) -> ((((a1,a2),a3),a4), ()),
    cmp = \((((a1,a2),a3),a4), ()) -> (((a1,a2),a3),a4)
}