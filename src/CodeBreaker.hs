module CodeBreaker where

import Data.List (group, sort, sortOn)
import Data.Maybe (fromMaybe)

frequencies :: String -> [(Char, Int)]
frequencies s = map (\x -> (head x, length x)) (group (sort s))

createSolver :: [(Char, Int)] -> Char -> Char
createSolver freqs =
  let sortedFreqs = sortOn snd freqs
      sortedChars = reverse (map fst sortedFreqs)
      solutionMap = zip sortedChars " etaoinsrhdlucmfywgpbvkxqjz"
  in \c -> fromMaybe c (lookup c solutionMap)

main = do
  code <- readFile "./src/code.txt"
  let solver = createSolver (frequencies code)
  print (take 100 (map solver code))
