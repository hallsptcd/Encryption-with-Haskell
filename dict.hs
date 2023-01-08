
import Data.Char
import Data.List
import System.IO
import System.Environment
import Prelude

main = do
-- Read the arguments from the command line
  [f1,f2,f3] <- getArgs
-- Assign the contents of f1, f2, and f3 to cont1, cont2, and cont3
  cont1 <- readFile f1
  cont2 <- readFile f2
  cont3 <- readFile f3
-- Append each of these long strings into the one list
  let combination = cont1 ++ " " ++ cont2 ++ " " ++ cont3
-- Include only regular letters and white space
  let combined = [c | c <- combination, isAlpha c || c == ' ']
-- Convert everything to lower case
  let word_list_lower = map toLower combined
-- Split the combined string into a list of words, using the in-built words function
  let words_list = words word_list_lower
-- Remove duplicates from the list of words, using in-built nub function
  let uniqueWords = nub words_list
-- Write the unique words to the output file, "dict.txt"
  writeFile "dict.txt" (unwords uniqueWords)



