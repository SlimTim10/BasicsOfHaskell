import Control.Applicative
import Control.Monad (liftM, ap)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = liftM
instance Applicative (State s) where
  pure = return
  (<*>) = ap
instance Monad (State s) where
  act >>= k = State $ \st ->
    let
      (x, st') = runState act st
      act' = k x
    in runState act' st'
  return x = State (\st -> (x, st))

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put newState = State $ \_ -> ((), newState)


type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
-- stackManip stack =
--   let
--     ((), newStack1) = push 3 stack
--     (a, newStack2) = pop newStack1
--   in
--     pop newStack2
stackManip = do
  push 3
  a <- pop
  pop
