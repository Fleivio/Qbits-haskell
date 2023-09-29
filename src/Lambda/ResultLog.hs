module Lambda.ResultLog(ResultLog(..), concatRes) where

type Log = String
data ResultLog a = Res {res :: a, log :: Log}

concatRes :: ResultLog a -> ResultLog a -> ResultLog a
concatRes (Res _ l1) (Res r l2) = Res r (l1 ++ l2)

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

