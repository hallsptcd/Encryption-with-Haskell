
import Data.Char (toLower)
import Data.List (group, sort)
import System.IO
import System.Environment

-- Here, we include an I/O version of the get_lang function.

get_lang :: String -> String
get_lang text = if de <= dp then "The text is in English" else "The text is in Portuguese" 
  where de = eucl_dist (freq_letter text) (eng_freq)  
        dp = eucl_dist (freq_letter text) (pt_freq)

main = do
  [f] <- getArgs
  s <- readFile f
  putStrLn (get_lang s)

-- We will require all of the other functions upon which get_lang depends.

freq_letter :: String -> [Float]
freq_letter s = map (\(_, n) -> 100 * fromIntegral n / total) counts
  where
    counts = countLetters s
    total = fromIntegral $ sum $ map snd counts

countLetters :: String -> [(Char, Int)]
countLetters s = [(c, count c s) | c <- ['a'..'z']]
  where
    count c = length . filter (== c) . map toLower 

eucl_dist :: Floating a => [a] -> [a] -> a
eucl_dist xs ys = distance
    where
      zipped = zip xs ys
      squaredDifferences = map (\(x, y) -> (x - y)^2) zipped
      sumSquaredDifferences = sum squaredDifferences
      distance = sqrt sumSquaredDifferences

eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]
