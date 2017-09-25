import Control.Applicative
import Control.Monad (liftM, ap)

instance Functor Trace where
  fmap = liftM
instance Applicative Trace where
  pure = return
  (<*>) = ap

newtype Trace a = Trace ([String], a)
  deriving Show

instance Monad Trace where
  (Trace (lst, x)) >>= k =
    let
      Trace (lst', x') = k x
    in
      Trace (lst ++ lst', x')
  return x = Trace ([], x)

put :: Show a => String -> a -> Trace ()
put msg v = Trace ([msg ++ " " ++ show v], ())

fact :: Integer -> Trace Integer
fact n = do
   put "fact" n
   if n == 0
       then return 1
       else do
           m <- fact (n - 1)
           return (n * m)

main = let Trace (lst, m) = fact 3
       in do
           print lst
           print m
