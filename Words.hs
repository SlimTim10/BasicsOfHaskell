import Data.Char
import Prelude hiding (Word)

type Word = String

sentence :: String -> [Word]
sentence "" = []
sentence str =
  let (w, str') = word str
  in w : sentence str'

-- returns a word and the rest of input
word :: String -> (Word, String)
word "" = ("", "")
word str =
  let
    (w, str') = span (not . isSpace) str
    (_, str'') = span isSpace str'
  in (w, str'')
                          
type Parser a = String -> (a, String)

several :: Parser a -> String -> [a]
several p "" = []
several p str =
  let (x, str') = p str
  in x : several p str'

num :: Parser Int
num str =
  let
    (digs, str') = span isDigit str
    (_, str'') = span (not . isDigit) str'
  in (read digs, str'')

main = print $ several num "12 4 128"
