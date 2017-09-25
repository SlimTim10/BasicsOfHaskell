module Calculator
  where

import qualified Data.Map as M

import Lexer (tokenize)
import Parser (parse)
import Evaluator

main :: IO ()
main =
  let initSymTab = M.fromList [("pi", pi)]
  in loop initSymTab

-- loop :: a -> IO ()
loop symTab = do
  str <- getLine
  if null str
    then
    return ()
    else
    let
      toks = tokenize str
      tree = parse toks
      Ev act = evaluate tree
      (val, symTab') = act symTab
    in do
      print val
      loop symTab'
      
-- test :: IO ()
-- test = (print . evaluate . parse . tokenize) "x1 = -15 / (2 + x2)"
