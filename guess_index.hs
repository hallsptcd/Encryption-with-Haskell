
import Data.Char
import Data.List
import System.IO
import System.Environment
import Prelude
import Control.Monad

-- We're going to use a function called loop to loop through different values of n, starting with 0
main = loop 0
  where
    loop n = do
-- Read the arguments from the command line
      [f1, f2] <- getArgs
-- Assign the contents of f1 and f2 to cont1 and cont2
      cont1 <- readFile f1
      cont2 <- readFile f2
-- Decode the encrypted file with the current value of n  
      let decoded = decode n cont2
-- Break the two files into lists of words
      let word_list1 = words cont1
      let word_list2 = words decoded
-- We don't want the index to go above the 26 letters in the alphabet
      if n > 26
        then return()
        else putStrLn $ "Checking n = " ++ show n
-- This is the metric we'll use to check the index. If the intersection of common words is above some value,
-- chose as 200 in this case, then it's very likely to be the correct encryption index. 200 was chosen as it's
-- high enough to be confident but not too high that the computation takes too long.
      if length (intersect word_list1 word_list2) > 200
        then putStrLn ("The encryption index, n, is: " ++ show n) 
        else loop (n+1)

-- The functions below are required for the decryption

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c + n) `mod` 26)
 | otherwise = c
 
decode :: Int -> String -> String
decode n s = [shift (-n) c | c <- s]