import Control.Applicative
import Control.Monad (liftM, ap)

-- data Either a b = Left a | Right b

alpha :: [(Char, Integer)]
alpha = zip ['a'..'g'] [1..]


-- Non-monadic first

myLookup :: (Eq a, Show a) => a -> [(a, b)] -> Either String b
myLookup x ys =
  case lookup x ys of
    Just v -> Right v
    Nothing -> Left $ "Could not find " ++ show x

myAdd2 :: Char -> Char -> Either String Integer
myAdd2 x y =
  case myLookup x alpha of
    Left msg -> Left msg
    Right x' ->
      case myLookup y alpha of
        Left msg -> Left msg
        Right y' -> Right $ x' + y'

myAdd3 :: Char -> Char -> Char -> Either String Integer
myAdd3 x y z =
  case myLookup x alpha of
    Left msg -> Left msg
    Right x' ->
      case myLookup y alpha of
        Left msg -> Left msg
        Right y' ->
          case myLookup z alpha of
            Left msg -> Left msg
            Right z' -> Right $ x' + y' + z'

myTest :: Either String Integer
myTest = myAdd3 'a' 'b' 'c'


-- Intermediate step to making a monad

-- We want bindE to be used like this:
interAdd2' :: Char -> Char -> Either String Integer
interAdd2' x y =
  bindE (myLookup x alpha)
  (\x' ->
     case myLookup y alpha of
       Left msg -> Left msg
       Right y' -> Right $ x' + y')

bindE :: Either String b
  -> (b -> Either String b)
  -> Either String b
bindE lk k =
  case lk of
    Left msg -> Left msg
    Right x -> k x

-- Define return and fail, since that's what we're doing
returnE :: b -> Either String b
returnE x = Right x

failE :: String -> Either String b
failE msg = Left msg

-- Use return and fail
interLookup :: (Eq a, Show a) => a -> [(a, b)] -> Either String b
interLookup x ys =
  case lookup x ys of
    Just v -> returnE v
    Nothing -> failE $ "Could not find " ++ show x

-- Using all of our monadic functions
interAdd2 :: Char -> Char -> Either String Integer
interAdd2 x y =
  bindE (myLookup x alpha)
  (\x' ->
     bindE (myLookup y alpha)
     (\y' ->
        return $ x' + y'))

interAdd3 :: Char -> Char -> Char -> Either String Integer
interAdd3 x y z =
  bindE (myLookup x alpha)
  (\x' ->
     bindE (myLookup y alpha)
     (\y' ->
        bindE (myLookup z alpha)
        (\z' ->
           return $ x' + y' + z')))

interTest :: Either String Integer
interTest = interAdd3 'a' 'b' 'c'


-- Making a monad instance

-- Define a new data type (to be a monad instance)
-- 'newtype' is the similar to 'data', but optimized
newtype Lookupable a = Lk (Either String a)
  deriving Show

-- Monad is a subclass of Applicative and Functor
instance Functor Lookupable where
  fmap = liftM
instance Applicative Lookupable where
  pure = return
  (<*>) = ap

-- Implement the monad functions
-- (>>=) is bind
instance Monad Lookupable where
  -- (>>=) :: m a -> (a -> m b) -> m b
  lk >>= k =
    case lk of
      Lk (Left msg) -> Lk (Left msg)
      Lk (Right x) -> k x
  -- return :: a -> m a
  return x = Lk (Right x)
  -- fail :: String -> m a
  fail msg = Lk (Left msg)


-- Using the monad

mLookup :: (Eq a, Show a) => a -> [(a, b)] -> Lookupable b
mLookup x ys =
  case lookup x ys of
    Just v -> return v
    Nothing  -> fail $ "Could not find " ++ show x

mAdd2' :: Char -> Char -> Lookupable Integer
mAdd2' x y =
  mLookup x alpha >>=
  (\x' ->
     mLookup y alpha >>=
     (\y' ->
        return $ x' + y'))

-- Using 'do' notation
mAdd2 :: Char -> Char -> Lookupable Integer
mAdd2 x y = do
  x' <- mLookup x alpha
  y' <- mLookup y alpha
  return $ x' + y'

mAdd3 :: Char -> Char -> Char -> Lookupable Integer
mAdd3 x y z = do
  x' <- mLookup x alpha
  y' <- mLookup y alpha
  z' <- mLookup z alpha
  return $ x' + y' + z'

mTest :: Lookupable Integer
mTest = mAdd3 'a' 'b' 'c'
