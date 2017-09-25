import qualified Data.Map as M
import Data.Char (toLower, isAlphaNum, isSpace)
import Data.List (sortBy)

type Index = M.Map String Int

indexWords :: Index -> [String] -> Index
indexWords index =
  foldl acc index
  where
    acc :: Index -> String -> Index
    acc ind w =
      let v = M.findWithDefault 0 w ind
      in M.insert w (v + 1) ind

splitWords :: String -> [String]
splitWords = words . cleanUp
  where
    cleanUp :: String -> String
    cleanUp = map (\c -> toLower c) . filter (\c -> isAlphaNum c || isSpace c)

mostFrequent :: [String] -> [(String, Int)]
mostFrequent wrds =
    let index = indexWords M.empty wrds
    in take 9 (sortBy cmpFreq (M.toList index))
  where
    cmpFreq :: (String, Int) -> (String, Int) -> Ordering
    cmpFreq (w1, n1) (w2, n2) = compare n2 n1

main = do
    text <- readFile "moby.txt"
    print $ mostFrequent (splitWords text)
