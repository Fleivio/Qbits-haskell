module Lambda.ResultLog(ResultLog(..)) where

type Log = String
data ResultLog a = Res {res :: a, log :: Log}

instance Show a => Show (ResultLog a) where
    show (Res _ s) = s

instance Eq a => Eq (ResultLog a) where 
    (Res a _) == (Res b _) = a == b

instance Functor ResultLog where
    fmap f (Res a s) = Res (f a) s

instance Applicative ResultLog where
    pure a = Res a ""
    (Res f s) <*> (Res a s') = Res (f a) (s ++ s')

instance Monad ResultLog where
    return = pure
    (Res a s) >>= f = let (Res b s') = f a in Res b (s ++ s')

