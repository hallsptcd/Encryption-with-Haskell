
import Data.Char
import Data.List
import System.IO
import System.Environment
import System.Random
import Data.Function

-- Useful Functions

-- First, I wrote a function to count the number of letters in a string, using a list comprehension.

cntlets :: String -> Int
cntlets str = length ( map toLower [c | c <- str, c `elem` ['a'..'z']] )

-- Next, I needed a function to count the number of individual letters in a string, and output the 
-- result as a list of tuples of the form (c, count), including letters not present in the string.

countLetters :: String -> [(Char, Int)]
-- Once again, I use a list comprehension to generate the list of tuples. 
countLetters s = [(c, count c s) | c <- ['a'..'z']]
  where
-- Here, I use the filter and map higher order functions, and compose the functions together, 
-- using map to convert all characters to lower case and filter to pull out matching characters.
    count c = length . filter (== c) . map toLower

-- Finally, we can use this countLetters function to generate the percentage of each letter.

freq_letter_pc :: String -> [(Char, Float)]
freq_letter_pc s = map (\(c, n) -> (c, 100 * fromIntegral n / total)) counts
  where
    counts = countLetters s
-- Here, I use the $ as a stand-in for parentheses.
    total = fromIntegral $ sum $ map snd counts

-- This function is a modified version of the previous, but outputting only the percentages, absent
-- the characters. This will be used in Question 5.

freq_letter :: String -> [Float]
freq_letter s = map (\(_, n) -> 100 * fromIntegral n / total) counts
  where
    counts = countLetters s
    total = fromIntegral $ sum $ map snd counts

-- Euclidean Distance

-- In order to compute the Euclidean distance in unlimited dimensions, I wrote a function that zips the two lists together, and computes the root of the sum of squared differences. It assumes the two lists are of equal size. 

eucl_dist :: Floating a => [a] -> [a] -> a
eucl_dist xs ys = distance
  where
    zipped = zip xs ys
    squaredDifferences = map (\(x, y) -> (x - y)^2) zipped
    sumSquaredDifferences = sum squaredDifferences
    distance = sqrt sumSquaredDifferences

-- In order to determine the language, we must compare the frequency distribution of the characters. 

-- First, I wrote a test function, to practise outputting strings.

message :: Int -> String
message t = if t < 3 then "Less than 3" else "Equal or more than 3" 

-- Here, we include the two language letter frequency distributions as functions in their own right.

eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]

-- Now, we can use the freq_letter and eucl_dist functions to determine the language of the string.
-- We do this by deterring the frequency distribution of characters in the input text, and compare
-- with the values known for each language.

get_lang :: String -> String
-- If the Euclidean distance between the text and the English distribution is smaller than the
-- distance between the text and the Portuguese distribution, then it's likely English. Otherwise,
-- it's likely Portuguese.
get_lang text = if de <= dp then "The text is in English" else "The text is in Portuguese" 
    where de = eucl_dist (freq_letter text) (eng_freq)  
          dp = eucl_dist (freq_letter text) (pt_freq)

-- We can adapt this function to accept command line instructions. This is in the attached file,
-- "get_lang.hs". Once compiled you can run "./get_lang file.txt". I have attached a file called 
-- chars.txt for use in this case. To compile, run "ghc --make get_lang.hs".

-- This decryption function is contained in another file call "c_decrypt.hs" which, once compiled, 
-- can be executed by typing "./c_decrypt file.txt n" where n is the shift index. The decrypted file
-- retains the full filename with the chp extension, plus "decrypted.chp" appended on top of that.

-- In order to determine the encryption index, n, we need to first create the dictionary of words 
-- contained in the novels. This is Part 1 of the question. This is done in the attached file, 
-- dict.hs. Next, we must use this dictionary to determine n. This is Part 2. This is done in the 
-- attached file, guess_index.hs. It works from the command line. Once compiled, type "./guess_index -- dict.txt encrypted.txt" where "encrypted.txt" is your encrypted file. The function will print the -- current value of the encryption index being tested, and then the correct encryption index when 
-- found.