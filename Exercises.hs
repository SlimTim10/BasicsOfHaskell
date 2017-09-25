newtype Reader e a = Reader (e -> a)

reader :: (e -> a) -> Reader e a
reader f = Reader f

runReader :: Reader e a -> e -> a
runReader (Reader act) env = act env

ask :: Reader e e
ask = reader (\e -> e)

instance Functor (Reader e) where
  fmap _ _ =  undefined
instance Applicative (Reader e) where
  pure _ = undefined
  _ <*> _ = undefined
instance Monad (Reader e) where
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- m <==> Reader e
  -- act :: m a <==> Reader e a
  -- k :: a ->  m b <==> a -> Reader e b
  rd >>= k = reader $ \env ->
    let
      x = runReader rd env
      act' = k x
    in
      runReader act' env
  return x = reader (\_ -> x)

test :: Reader String Int
test = do
  s <- ask
  return $ read s + 1

main = print $ runReader test "13"
