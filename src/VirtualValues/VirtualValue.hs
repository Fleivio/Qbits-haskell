module VirtualValues.VirtualValue (virtFromR, virtFromV, app, app1, Virt(..), observeVV) where

import Basic.Reference ( QR(..) )
import Basic.Basis ( Basis(..) )
import Basic.Operators ( normalize, qApp, qop, Qop(..) )
import Data.IORef ( readIORef, writeIORef )
import Basic.QuantumValue ( getProb, toQv )
import Basic.ProbabilityAmplitude ( squareModulus )
import Basic.Observation ( observeV )
import Data.Complex ( Complex((:+)) )

import VirtualValues.Adaptor ( Adaptor(..) )

-- Valor virtual que recebe o contexto inteiro e uma funcao que filtra o qbit que temos interesse
data Virt a na ua = Virt (QR ua) (Adaptor (a, na) ua)

-- Cria um valor virtual a partir de uma unica referencia
virtFromR :: QR a -> Virt a () a
virtFromR r = Virt r (Adaptor {dec = \a -> (a,()), cmp = \(a,()) -> a})

-- Compoe as funções de um adaptador com outro
virtFromV :: Virt a na u -> Adaptor (a1, a2) a -> Virt a1 (a2, na) u
virtFromV (Virt r gAdaptor) lAdaptor = Virt r composedAdaptor
    where
        gdec = dec gAdaptor
        gcmp = cmp gAdaptor
        ldec = dec lAdaptor
        lcmp = cmp lAdaptor
        composedAdaptor = Adaptor {dec = composedDec, cmp = composedCmp}
        composedCmp (a1, (a2, na)) = gcmp (lcmp (a1,a2), na)
        composedDec u = let (a, na) = gdec u
                            (a1, a2) = ldec a
                        in (a1, (a2, na))

app :: (Basis a, Basis b, Basis nab, Basis ua, Basis ub) => Qop a b -> Virt a nab ua -> Virt b nab ub -> IO()
app (Qop f)
    (Virt (QR ra) (Adaptor {dec = deca, cmp = _}))
    (Virt (QR rb) (Adaptor {dec = decb, cmp = _})) =
    do 
        fa <- readIORef ra
        let fb = normalize $ qApp gf fa
        writeIORef rb fb

    where gf = qop [ ( (ua, ub), getProb f (a, b) ) | (ua, ub) <- basis,
                    let (a, na) = deca ua
                        (b, nb) = decb ub,
                    na == nb ]

app1 :: (Basis a, Basis na, Basis ua) => Qop a a -> Virt a na ua -> IO ()
app1 f v = app f v v


-- Observa um valor virtual e colapsa as probabilidade da totalidade do contexto
observeVV :: (Basis a, Basis na, Basis ua) => Virt a na ua -> IO a
observeVV (Virt (QR r) (Adaptor {dec = dec1, cmp = cmp1})) = 
    do 
        qVal <- readIORef r
        let virtualProb a = sqrt. sum $ [squareModulus (getProb qVal (cmp1(a, na))) :+ 0 | na <- basis]
            virtualQVal = toQv [ (a, virtualProb a) | a <- basis]
        observResult <- observeV virtualQVal
        let nv = toQv [ (u, getProb qVal (cmp1 (observResult, na))) | u <- basis, let (a, na) = dec1 u, a == observResult ]
        writeIORef r $ normalize nv
        return observResult
