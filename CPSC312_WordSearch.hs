
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

-- BUILD FUNCTIONS AFTER THIS 
-- Function to generate a random letter
randomLetter :: IO Char
randomLetter = randomRIO ('a', 'z')

-- Function to fill the word search grid with random letters
fillGrid :: Int -> Int -> [[Char]] -> IO [[Char]]
fillGrid rows cols grid =
  if rows == 0
  then return grid
  else do
    newRow <- sequence [randomLetter | i <- [1..cols]]
    fillGrid (rows-1) cols (newRow : grid)

-- Function to place a word in the word search grid
placeWord :: String -> [[Char]] -> IO [[Char]]
placeWord word grid = do
  let rows = length grid
  let cols = length (head grid)
  let wordLen = length word
  let orientations = [(0, 1), (1, 0), (1, 1), (-1, 1)]
  let validPositions = [(i, j, o) | i <- [0..rows-1], j <- [0..cols-1], o <- orientations,
                        let endRow = i + (wordLen - 1) * fst o,
                        let endCol = j + (wordLen - 1) * snd o,
                        endRow >= 0 && endRow < rows && endCol >= 0 && endCol < cols]
  if null validPositions
  then return grid
  else do
    let (i, j, o) = head validPositions
    let newGrid = [[if i' == row && j' == col
                    then if (i' - i) `mod` (fst o) == 0 && (j' - j) `mod` (snd o) == 0
                         then word !! ((i' - i) `div` (fst o) + (j' - j) `div` (snd o))
                         else grid !! row !! col
                    else grid !! row !! col
                  | col <- [0..cols-1]]
                 | row <- [0..rows-1]]
    return newGrid

-- Function to generate the word search game
generateWordSearch :: [String] -> IO ()
generateWordSearch words = do
  let gridSize = 15
  grid <- fillGrid gridSize gridSize [[] | i <- [1..gridSize]]
  filledGrid <- foldrM placeWord grid words
  mapM_ putStrLn filledGrid

-- Main function to get input and generate the word search
main :: IO ()
main = do
  putStrLn "Enter a list of words to include in the word search, separated by commas:"
  input <- getLine
  let words = words (map (\c -> if c == ',' then ' ' else c) input)
  generateWordSearch words