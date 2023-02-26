
-- Word Search
import System.Random

data WordSearch = Row [Char]
  | Rows [WordSearch]
  
generateSearch :: [String] -> IO WordSearch
  
--Takes a set of words and returns a word search with a minimum of 25 characters in the list of searchable words
generateSearch [] = generateSearch (randomwords 25)
generateSearch ourwords
 | (sum( map length ourwords)) < 25 = generateSearch (ourwords++(randomwords (25 - (sum( map length ourwords)))))
 | otherwise = do putStr (buildRows ourwords)


--Takes in a number of characters that are needed as extra and returns a list of words whose length sum to that integer
randomwords :: Int -> [String]

randomwords 0 = []
randomwords 1 = []
randomwords 2 = []
randomwords n 
 | n `mod` 5 == 0 = pickfive (fromIntegral(round(n/5)))
 | n `mod` 5 == 1 = (pickword 6): randomwords (n-6)
 | n `mod` 5 == 2 = (pickword 7): randomwords (n-7)
 | n `mod` 5 == 3 = (pickword 8): randomwords (n-8)
 | n `mod` 5 == 4 = (pickword 9): randomwords (n-9)
 

--Takes an integer from 6-9 and returns a random word of that length from a list of words
pickword:: Int -> String

pickword n 
 | n < 6 = []
 | n == 6 = "Strong"
 | n == 7 = "Freedom"
 | n == 8 = "Hospital"
 | n == 9 = "Brilliant"
 

--Takes an integer from 0-5 and returns that many 5 letter words in a list
pickfive :: Int -> [String]

pickfive n 
 | n > 5 = pickfive 5
 | otherwise = take n ["apple", "beach", "chess", "daisy", "event"]
 
--Takes a set of words and returns a set of rows
buildRows :: [String] -> WordSearch

=======
-- Word Search
import System.Random

data WordSearch = Row [Char]
  | Rows [WordSearch]
  
generateSearch :: [String] -> IO WordSearch
  
--Takes a set of words and returns a word search with a minimum of 25 characters in the list of searchable words
generateSearch [] = generateSearch (randomwords 25)
generateSearch ourwords
 | (sum( map length ourwords)) < 25 = generateSearch (ourwords++(randomwords (25 - (sum( map length ourwords)))))
 | otherwise = do putStr (buildRows ourwords)


--Takes in a number of characters that are needed as extra and returns a list of words whose length sum to that integer
randomwords :: Int -> [String]

randomwords 0 = []
randomwords 1 = []
randomwords 2 = []
randomwords n 
 | n `mod` 5 == 0 = pickfive (fromIntegral(round(n/5)))
 | n `mod` 5 == 1 = (pickword 6): randomwords (n-6)
 | n `mod` 5 == 2 = (pickword 7): randomwords (n-7)
 | n `mod` 5 == 3 = (pickword 8): randomwords (n-8)
 | n `mod` 5 == 4 = (pickword 9): randomwords (n-9)
 

--Takes an integer from 6-9 and returns a random word of that length from a list of words
pickword:: Int -> String

pickword n 
 | n < 6 = []
 | n == 6 = "Strong"
 | n == 7 = "Freedom"
 | n == 8 = "Hospital"
 | n == 9 = "Brilliant"
 

--Takes an integer from 0-5 and returns that many 5 letter words in a list
pickfive :: Int -> [String]

pickfive n 
 | n > 5 = pickfive 5
 | otherwise = take n ["apple", "beach", "chess", "daisy", "event"]
 
--Takes a set of words and returns a set of rows
buildRows :: [String] -> WordSearch

buildRows _ = Rows [] 