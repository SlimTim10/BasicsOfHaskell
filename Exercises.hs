import Control.Applicative
import Control.Monad (liftM, ap)

data WhyNot a = Nah | Sure a
  deriving Show

instance Functor WhyNot where
  fmap = liftM
instance Applicative WhyNot where
  pure = return
  (<*>) = ap

instance Monad WhyNot where
   x >>= k =
     case x of
       (Sure x') -> k x'
       Nah -> Nah
   return x = Sure x
   fail x = Nah

safeRoot :: Double -> WhyNot Double
safeRoot x = 
    if x >= 0 then 
      return (sqrt x)
    else
      fail "Boo!"

test :: Double -> WhyNot Double
test x = do
   y <- safeRoot x
   z <- safeRoot (y - 4)
   w <- safeRoot z
   return w


main = do
    print $ test 9
    print $ test 400

