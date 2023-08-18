module Virtual.VirtualValue (virtFromR, virtFromV, app, app1, Virt(..), virtTensor, observeVV, virtOne, virtZero) where

import Quantum.Value
import Quantum.Operators
import Reference.Reference
import Reference.Observation 

import Virtual.Adaptor ( Adaptor(..) )
import GHC.IO (unsafePerformIO)

-- Valor virtual que recebe o contexto inteiro e uma funcao que filtra o qbit que temos interesse
data Virt a rest global = Virt (QR global) (Adaptor (a, rest) global)

instance (Show global) => Show (Virt a rest global) where
    show (Virt ref _ ) = show ref

virtOne :: Virt Bool () Bool
virtOne = virtFromR $ unsafePerformIO $ mkQR (mkQV [(True, 1)])

virtZero :: Virt Bool () Bool
virtZero = virtFromR $ unsafePerformIO $ mkQR (mkQV [(False, 1)]) 

-- Cria um valor virtual a partir de uma unica referencia
virtFromR :: QR a -> Virt a () a
virtFromR r = Virt r (Adaptor (\a -> (a,())) (\(a,()) -> a))

-- Compoe as funções de um adaptador com outro
virtFromV ::
    Virt a rest u -> Adaptor (a1, a2) a -> Virt a1 (a2, rest) u
virtFromV (Virt r gAdaptor) lAdaptor = Virt r composedAdaptor
    where
        gdec = dec gAdaptor
        gcmp = cmp gAdaptor
        ldec = dec lAdaptor
        lcmp = cmp lAdaptor
        composedAdaptor = Adaptor composedDec composedCmp
        composedCmp (a1, (a2, rest)) = gcmp (lcmp (a1,a2), rest)
        composedDec u = let (a, rest) = gdec u
                            (a1, a2) = ldec a
                        in (a1, (a2, rest))

app :: (Basis a, Basis b, Basis restb, Basis global, Basis ub) 
    => Qop a b -> Virt a restb global -> Virt b restb ub -> IO()
app (Qop f _)
    (Virt (QR ra) (Adaptor deca _))
    (Virt (QR rb) (Adaptor decb _)) =
    do 
        fa <- readIORef ra
        let fb = normalize $ qApp gf fa
        writeIORef rb fb

    where gf = qop [ ( (global, ub), getProb f (a, b) ) | (global, ub) <- basis,
                    let (a, rest) = deca global
                        (b, nb) = decb ub,
                    rest == nb ] "combined"

app1 :: (Basis a, Basis rest, Basis global) => Qop a a -> Virt a rest global -> IO ()
app1 f v = app f v v

-- Observa um valor virtual e colapsa as probabilidade da totalidade do contexto
observeVV :: (Basis a, Basis rest, Basis global) => Virt a rest global -> IO a
observeVV (Virt (QR r) (Adaptor dec1 cmp1)) = 
    do 
        qVal <- readIORef r
        let virtualProb a = sqrt. sum $ [squareModulus (getProb qVal (cmp1(a, rest))) `addPA` 0 | rest <- basis]
            virtualQVal = mkQV [ (a, virtualProb a) | a <- basis]
        observResult <- observeV virtualQVal
        let nv = mkQV [ (u, getProb qVal (cmp1 (observResult, rest))) | u <- basis, let (a, rest) = dec1 u, a == observResult ]
        writeIORef r $ normalize nv
        return observResult

virtTensor :: ( Basis globala, Basis globalb)
    =>Virt a resta globala -> Virt b restb globalb -> IO (Virt (a, b) (resta, restb) (globala, globalb))
virtTensor (Virt ra (Adaptor deca cmpa))
           (Virt rb (Adaptor decb cmpb)) =
    do
        rc <- ra &&* rb
        let decc (ga, gb) = ((a,b), (ra', rb'))
                where (a, ra') = deca ga
                      (b, rb') = decb gb
            cmpc ((a,b), (ra', rb')) = (ga', gb')
                where ga' = cmpa (a, ra')
                      gb' = cmpb (b, rb')
            adaptC = Adaptor decc cmpc

        return $ Virt rc adaptC