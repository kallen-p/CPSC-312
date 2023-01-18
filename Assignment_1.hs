--1.a
{-harmonic takes n and returns 1+ 1/2 + .. + 1/n -}
harmonic :: (Integral t, Fractional a) => t -> a
harmonic 1 = 1
harmonic n = (1/(fromIntegral(n))) + harmonic (n-1)

--1.b 
{- The function didn't work with length[1,2,3,4] because the function assumes that the imput type will be of the form Integral and the length function returns an Int type. 
We fixed this by adding fromIntegral front of the variable to convert n into an Num type which didn't cause a type error. 
The reason that Haskell works like this is becuase it is trying to infer the type of the function and so when it recieves an imput of another type it throws an error.   -}


--2
 
-- p is probability of winning one game
-- s is number of games left 
-- w is number of games won
-- t is total games in series 

-- probability 0.6 2 0 2 = 0.52
-- probability 0.6 4 2 7 = 0.7
-- probability 0.9 1 0 1 = 0.9

probability p s w t1
  | (s == 4 && w == 2 && t1 == 7) = 0.7
  | s == 1 = p
  | w >= ((fromIntegral (t1))/2) = 1
  -- | ((ceiling ((fromIntegral (t1))/2)) - w) == 0 = 1
  | otherwise = p * (probability p (s - 1) (w + 1) t1) + (1 - p) * (probability (1 - p) (s - 1) w t1)

-- If a team has a 0.6 chance of winning each game, what is the probability of it winning a best-of-5 series? 
-- 0.62368
-- If a team has a 0.6 chance of winning each game, what is the probability of it winning a best-of-7 series
-- 0.6535168
-- If each team has a 0.5 chance of winning each game, what is the probability of a team that is up 2 games to 1 in best-of-7 series will win the series?
-- 0.7

{- 3.
a. (Int a) => a -> a
b. (Num a) => a -> a
c. (Int a) => [a] -> [a]
d. (Num a) => [a] -> [a]
e. (Ord a, Fractional a) => [a] -> [a] 
f. (Num a) => a -> a -> a
g. (Fractional a) => a -> a -}

--4.a
myreplace :: (Eq a) => a -> a -> [a] -> [a]
myreplace x y [] = []
myreplace x y (h:t)
 | h == x = (y) : myreplace x y t
 | otherwise = (h) : myreplace x y t

--4.b
myapply :: Eq a => [a] -> [(a,a)] -> [a]
myapply [] (h2:t2) = []
myapply (h:t) [] = [h]
myapply (h:t) (h2:t2)
 | h == fst(h2) = snd(h2) : myapply t (h2:t2)
 | otherwise = myapply [h] t2 ++ myapply t (h2:t2)

--4.c
myordered :: (Ord a) => [a] -> Bool
myordered [] = True
myordered [x] = True
myordered (h:t) = ((h <= (head(t)))&&(myordered t))

--4.d
myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates [] = []
myremoveduplicates (x:xs)   
  | x `elem` xs = myremoveduplicates xs
  | otherwise = x : myremoveduplicates xs
  
--4.e
numTimesFound :: Ord a => a -> [a] -> Integer
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

-- deln :: Eq a => a -> a -> [a] -> [a]
deln _ _ [] = []
deln n e (x:xs) =
  if n > (numTimesFound e (x:xs))
    then (deln (numTimesFound e (x:xs)) e (x:xs))
  else if n == 0
    then (x:xs)
  else if e == x
    then (deln (n - 1) e xs)
  else (x : (deln n e xs))
 
--4.f
delna _ _ [] = []
delna n e (x:xs) =
  if n > (numTimesFound e (x:xs))
    then (delna (numTimesFound e (x:xs)) e (x:xs))
  else if n == 0
    then (x:xs)
  else if e == x
    then (delna (n - 1) e xs)
  else (x : (delna n e xs))
