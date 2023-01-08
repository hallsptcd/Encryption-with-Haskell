
-- /Users/simon/Desktop/Data_Science/Programming_Paradigms/Haskell

import Data.Char
import Data.List
import System.IO
import System.Environment
import System.Random
import Data.Function

-- Question 1

-- We can check if a number, n, is a perfect square by successively squaring the natural numbers, 
-- beginning with 1 and going until n, and checking if any of these values are equal to n. I suppose
-- we needn't search all the way up to n itself, but so long as n isn't too large, this isn't such a
-- major issue.

is_square :: Int -> Bool
-- I make use of an intermediate function, check_square i, initialised with a value of i = 1.
is_square n = check_square 1
  where
-- Here. We can make use of guards to return True if the square of i is the same as n, False if
-- the square of i goes above n, and otherwise call the check_square again while incrementing i.
    check_square i
      | i * i == n = True
      | i * i > n = False
      | otherwise = check_square (i + 1)

-- Question 2

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

-- Question 3

-- First, I need some functions which output the first, second, third, and fourth elements of a list.

-- In all of these, I use the anonymous variable for variables that are not required.
first :: (a, b, c, d) -> a
first (x, _, _, _) = x

second :: (a, b, c, d) -> b
second (_, x, _, _) = x

third :: (a, b, c, d) -> c
third (_, _, x, _) = x

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

-- A) 

-- Now, we can write a function to output the cities above a certain population, n. I have chosen
-- to include the cities list of tuples inside the function itself.

get_city_above :: Int -> [String]
get_city_above n = 
  let cities = [(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3), (1, "Edinburgh",500000,2),(1,"Florence",50000,3), (1,"Venice",200000,3), (1,"Lyon",1000000,1), (1,"Milan",3000000,3), (1,"Madrid",6000000,4), (1,"Barcelona",5000000,4)]
      countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")]
-- We can use a list comprehension to generate the list.
   in [second c | c <- cities, third c > n]

-- B)

-- Now, to get all the cities given the country_name, I first need to write a function which takes
-- the country name and retrieves the country_id. I call this findNumber.

findNumber :: String -> [(Int, String)] -> Int
findNumber name countries = head [number | (number, country) <- countries, country == name]

get_city :: String -> [String]
get_city country = 
  let cities = [(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3), (1, "Edinburgh",500000,2),(1,"Florence",50000,3), (1,"Venice",200000,3), (1,"Lyon",1000000,1), (1,"Milan",3000000,3), (1,"Madrid",6000000,4), (1,"Barcelona",5000000,4)]
      countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")]
      country_id = findNumber country countries
   in [second c | c <- cities, country_id == fourth c]

-- C)

-- In order to produce a list of tuples containing the country name and the number of cities we
-- first need a function to find the number of cities.

cities_num :: String -> Int
cities_num country = length (get_city country)

num_city :: [(String, Int)]
num_city = 
  let cities = [(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3), (1, "Edinburgh",500000,2),(1,"Florence",50000,3), (1,"Venice",200000,3), (1,"Lyon",1000000,1), (1,"Milan",3000000,3), (1,"Madrid",6000000,4), (1,"Barcelona",5000000,4)]
      countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")]
   in [(snd(c), cities_num(snd(c))) | c <- countries]

-- Question 4

-- In order to compute the Euclidean distance in unlimited dimensions, I wrote a function that zips the two lists together, and computes the root of the sum of squared differences. It assumes the two lists are of equal size. 

eucl_dist :: Floating a => [a] -> [a] -> a
eucl_dist xs ys = distance
  where
    zipped = zip xs ys
    squaredDifferences = map (\(x, y) -> (x - y)^2) zipped
    sumSquaredDifferences = sum squaredDifferences
    distance = sqrt sumSquaredDifferences

-- Question 5

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

-- Question 6

-- This decryption function is contained in another file call "c_decrypt.hs" which, once compiled, 
-- can be executed by typing "./c_decrypt file.txt n" where n is the shift index. The decrypted file
-- retains the full filename with the chp extension, plus "decrypted.chp" appended on top of that.

-- Question 7

-- In order to determine the encryption index, n, we need to first create the dictionary of words 
-- contained in the novels. This is Part 1 of the question. This is done in the attached file, 
-- dict.hs. Next, we must use this dictionary to determine n. This is Part 2. This is done in the 
-- attached file, guess_index.hs. It works from the command line. Once compiled, type "./guess_index -- dict.txt encrypted.txt" where "encrypted.txt" is your encrypted file. The function will print the -- current value of the encryption index being tested, and then the correct encryption index when 
-- found.

-- Question 8

-- In order to approximate the area of a quarter unit circle, we need to generate points in the 
-- first quadrant. This function is contained in the attached file, montecarlo.hs. Simply run
-- it as follows: "./montecarlo".

-- I opted to generate random pairs, (x, y), of numbers in the range 0 <= x <= 1 and 0 <= y <= 1,
-- and to calculate the area of the quarter unit circle by finding the ratio of the number of pairs
-- that fall inside the circle to the total number of pairs. I opted to generate 50,000 pairs, but
-- this number can be increased for improved accuracy. The correct area of the circle ought to be
-- pi/4 = 0.78539. With 50,000 pairs of random numbers, we get an approximate area of 0.7897.

-- Question 9

-- In this question, I played around with different options, including having a Float -> Float 
-- signature, but I settled on Int -> Float as this more neatly matches the real input. I'll handle
-- the data type issues inside the functions.

-- This is the first sample series
sample_series :: Int -> Float
sample_series k = 1 / (2^k)

-- This is the first math_series higher-order function. With the sample series above, we need to
-- sum from 0 in order for the first term to be 1, as we'd expect. We must therefore sum to n-1.
math_series :: (Int -> Float) -> Int -> Float
math_series series_fn n = sum $ map series_fn [0..n-1]

-- This is the series which converges to pi as k approaches infinity. I had to convert the integer
-- k to a float in order to carry out the arithmetic.
series :: Int -> Float
series k = ((-1)^(k+1)) * (4 / ((2*k_float) - 1))
  where 
    k_float = fromIntegral k

-- This is the main math_series higher-order function. In this case, we begin the sum with k = 1,
-- and go to n. The higher the value of n, the closer the convergence to pi. Choosing n = 10,
-- we get a value of 3.04. Choosing n = 10000, we get 3.14149. With n = 100000, it's 3.14158.
math_series_pi :: (Int -> Float) -> Int -> Float
math_series_pi series_fn n = sum $ map series_fn [1..n]

-- Question 10

-- We must define the function in the argument, and we must also use doubles for both x1 and x2, e.g.
-- "integral (\x -> 0.5*x) 0.0 20.0 50" which produces an estimate of about 102, close to
-- the real value of exactly 100. Choosing "integral (\x -> 0.5*x) 0.0 20.0 50000" it becomes 
-- 100.002.

integral :: (Double -> Double) -> Double -> Double -> Int -> Double
integral f x1 x2 n =
-- We must convert n from Int
  let dx = (x2 - x1) / fromIntegral n
      xs = [x1, x1 + dx .. x2]
-- I use an anonymous function to apply the function, f
  in sum (map (\x -> f x * dx) xs)
