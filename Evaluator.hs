module Evaluator (evaluate, Evaluator(..))
  where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad (liftM, ap)

import Parser (Tree(..))
import Lexer (Operator(..))

type SymTab = M.Map String Double

newtype Evaluator a = Ev (SymTab -> (a, SymTab))

instance Functor Evaluator where
  fmap = liftM
instance Applicative Evaluator where
  pure = return
  (<*>) = ap
instance Monad Evaluator where
  (Ev act) >>= k = Ev $ \symTab ->
    let
      (x, symTab') = act symTab
      (Ev act') = k x
    in
      act' symTab'
  return x = Ev (\symTab -> (x, symTab))

evaluate :: Tree -> Evaluator Double
evaluate (SumNode op left right) = do
  left' <- evaluate left
  right' <- evaluate right
  case op of
    Plus -> return (left' + right')
    Minus -> return (left' - right')
evaluate (ProdNode op left right) = do
  left' <- evaluate left
  right' <- evaluate right
  case op of
    Times -> return (left' * right')
    Div -> return (left' / right')
evaluate (UnaryNode op tree) = do
  x <- evaluate tree
  case op of
    Plus -> return x
    Minus -> return (-x)
evaluate (NumNode x) = return x
evaluate (AssignNode str tree) = do
  v <- evaluate tree
  addSymbol str v
  return v
evaluate (VarNode str) = lookUp str

lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symTab ->
  case M.lookup str symTab of
    Just v -> (v, symTab)
    Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symTab ->
  let symTab' = M.insert str val symTab
  in (val, symTab')
