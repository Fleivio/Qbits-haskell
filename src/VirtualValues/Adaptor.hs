module VirtualValues.Adaptor (Adaptor(..), ad_triple13, ad_triple23, ad_triple12, ad_triple3, ad_triple2, ad_triple1, ad_triple, ad_pair2, ad_pair1) where

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

ad_triple1 :: Adaptor (a1, (a2, a3)) (a1, a2, a3)
ad_triple1 = Adaptor {
    dec = \(a1, a2, a3) -> (a1, (a2, a3)),
    cmp = \(a1, (a2, a3)) -> (a1, a2, a3) 
}

ad_triple2 :: Adaptor (a2, (a1, a3)) (a1, a2, a3)
ad_triple2 = Adaptor {
    dec = \(a1, a2, a3) -> (a2, (a1, a3)),
    cmp = \(a2, (a1, a3)) -> (a1, a2, a3)
}

ad_triple3 :: Adaptor (a3, (a1, a2)) (a1, a2, a3) 
ad_triple3 = Adaptor {
    dec = \(a1, a2, a3) -> (a3, (a1, a2)),
    cmp = \(a3, (a1, a2)) -> (a1, a2, a3)
}

ad_triple12 :: Adaptor ((a1, a2), a3) (a1, a2, a3)
ad_triple12 = Adaptor {
    dec = \(a1, a2, a3) -> ((a1, a2), a3),
    cmp = \((a1, a2), a3) -> (a1, a2, a3) 
}

ad_triple13 :: Adaptor ((a1, a3), a2) (a1, a2, a3)
ad_triple13 = Adaptor {
    dec = \(a1, a2, a3) -> ((a1, a3), a2),
    cmp = \((a1, a3), a2) -> (a1, a2, a3) 
}

ad_triple23 :: Adaptor ((a2, a3), a1) (a1, a2, a3)
ad_triple23 = Adaptor {
    dec = \(a1, a2, a3) -> ((a2, a3), a1),
    cmp = \((a2, a3), a1) -> (a1, a2, a3) 
}

ad_triple :: Adaptor ((a1,a2,a3), ()) (a1,(a2,a3))
ad_triple = Adaptor {
    dec = \(a1, (a2, a3)) -> ((a1,a2,a3), ()),
    cmp = \((a1, a2, a3), ()) -> (a1,(a2,a3))
}