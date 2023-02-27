
-- Word Search
import System.Random
import Data.Foldable

type WsChar = (Char, Bool)

data WordSearch = Row [WsChar]
  | Rows [WordSearch]
 

generateSearch :: [String] -> IO ()
  
--Takes a set of words and returns a word search with a minimum of 25 characters in the list of searchable words
generateSearch [] = generateSearch (randomwords 25)
generateSearch ourwords
 | (sum( map length ourwords)) < 25 = generateSearch (ourwords++(randomwords (25 - (sum( map length ourwords)))))
 | otherwise = mapM_ putStrLn (buildRows ourwords)


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
buildRows :: [String] -> IO WordSearch

buildRows ourwords = do
  let numrows = 15
  randWS <- fillWS numrows numrows []
  filledWS <- placeWords randWS ourwords randWS ourwords
  return filledWS

--Generates a random letter from a to z
randomLetter :: IO Char
randomLetter = randomRIO ('a', 'z')

--Fills a nxn sized wordsearch with random letters 
fillWS :: Int -> Int -> WordSearch

fillWS numrows numcols ws =
 if numrows == 0 
  then return ws
  else do
   let newRow = [(randomLetter, True) | i <- [1..numcols]]
   fillWS (numrows - 1) numcols (newRow : ws)


placeWords :: [String] -> WordSearch -> [String] -> WordSearch -> IO WordSearch

placeWords [] ws ogwords ogws= return ws
placeWords ourwords ws ogwords ogws = 
 do 
  let temp = checkPosValid length(head(ourwords)) (randInt,randInt) ws
  let isvalid = fst temp
  let pos = snd temp
  let ori = last temp
     if isvalid
	  then placeWords tail(ourwords) (placeChar head(ourwords) ws pos ori) ogwords ogws
      else placeWords ogwords ogws ogwords ogws

--Checks if a word can be placed in a radnom orientation given a position, wordlength and a wordsearch.
checkPosValid :: Int -> (Int,Int) -> WordSearch -> (Bool, (Int, Int), (Int, Int))

checkPosValid wordlength pos ws =
  let ori = randomOrientation -- Define or provide a value for `randomOrientation`
      rowpos = fst pos
      colpos = snd pos
  in if snd (ws !! rowpos !! colpos)
       then do
         let positions = [(i * fst ori + rowpos, i * snd ori + colpos) | i <- [1..wordlength]]
         valid <- sequenceA [checkSafeRow ws x y | (x, y) <- positions]
         if isJust valid
           then if all (snd(ws!!x!!y))
           else return (False, pos, ori)
       else return (False, pos, ori)

--Checks if the word will fit vertically and horizontally in the wordsearch
checkSafeRow :: WordSearch -> Int -> Int -> Maybe Bool
checkSafeRow ws x y = case ws of
  [] -> Nothing
  (row:rows) ->
    if x == 0
      then checkSafeCol row y
      else checkSafeRow rows (x - 1) y >>= checkSafeCol row y
	  
--Checks if the word will fit horizontally in a row of the wordsearch
checkSafeCol :: WordSearch -> Int -> Maybe Bool
checkSafeCol row i = if i >= 0 && i < length row
                    then Just True
                    else Nothing

{-
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
  let orientations = [(0, 1), (1, 0), (1, 1), (-1, 1), (0, -1), (-1, 0), (-1, -1)]
  let validPositions = [(i, j, o) | i <- [0..rows-1], j <- [0..cols-1], o <- orientations,
                        let endRow = i + (wordLen - 1) * fst o,
                        let endCol = j + (wordLen - 1) * snd o,
                        endRow >= 0 && endRow < rows && endCol >= 0 && endCol < cols]
  if null validPositions
  then return grid
  else do
    let (i, j, o) = head validPositions
    let newGrid = [[if i' == row && j' == cols
                     then if (i' - i) `mod` (fst o) == 0 && (j' - j) `mod` (snd o) == 0
                     then word !! ((i' - i) * (fst o) + (j' - j) * (snd o))
                     else grid !! row !! cols
                else grid !! row !! cols
              | j' <- [j..(j + (wordLen - 1) * snd o)],
                i' <- [i..(i + (wordLen - 1) * fst o)],
                i' >= 0, j' >= 0, i' < rows, j' < cols,
                (i' - i) `mod` (fst o) == 0 || (j' - j) `mod` (snd o) == 0]
             | row <- [0..rows-1]]
    return newGrid

-- Function to generate the word search game
generateWordSearch :: [String] -> IO ()
generateWordSearch ourwords = do
  let gridSize = 15
  grid <- fillGrid gridSize gridSize [[] | i <- [1..gridSize]]
  filledGrid <- foldrM placeWord grid ourwords
  mapM_ putStrLn filledGrid

-- Main function to get input and generate the word search
main :: IO ()
main = do
  putStrLn "Enter a list of words to include in the word search, separated by commas:"
  input <- getLine
  let ourwords = words (map (\c -> if c == ',' then ' ' else c) input)
  generateWordSearch ourwords -}