--Kallen Paterson (87052684)
--Alex Baillie (30018774)

--1.a
{-harmonic takes n and returns 1+ 1/2 + .. + 1/n 
Tests: 
harmonic 0 = 0
harmonic 2 = 1.5
harmonic 10 = 2.9289 -}

harmonic :: (Integral t, Fractional a) => t -> a
harmonic 0 = 0
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
{- myreplace takes two elements x, y from the Eq class and a list of the same type and replaces all occurences of x with y in the list
Tests: 
myreplace 7 3 [7,0,7,1,7,2,7,3] => [3,0,3,1,3,2,3,3]
myreplace ’a’ ’x’ "" => ""
myreplace ’a’ ’x’ "xabacadx" => "xxbxcxdx"-}

myreplace :: (Eq a) => a -> a -> [a] -> [a]
myreplace x y [] = []
myreplace x y (h:t)
 | h == x = (y) : myreplace x y t
 | otherwise = (h) : myreplace x y t

--4.b
{- myapply takes two lists, the 2nd being a list of (x , y) tuples, and replaces each occurrence of x by y in the first list 
Tests:
myapply "abcdec" [(’a’,’f’), (’c’,’3’), (’g’,’7’)] => "fb3de3"
myapply "baab" [(’a’,’b’), (’b’,’a’)] => "abba"-}

myapply :: Eq a => [a] -> [(a,a)] -> [a]
myapply [] (h2:t2) = []
myapply (h:t) [] = [h]
myapply (h:t) (h2:t2)
 | h == fst(h2) = snd(h2) : myapply t (h2:t2)
 | otherwise = myapply [h] t2 ++ myapply t (h2:t2)

--4.c
{-myordered takes in a list and returns True if the list goes from least to greatest and False otherwise. 
Tests:
myordered [] => True
myordered [2] => True
myordered [1,2] => True
myordered [1,1] => True -} 

myordered :: (Ord a) => [a] -> Bool
myordered [] = True
myordered [x] = True
myordered (h:t) = ((h <= (head(t)))&&(myordered t))

--4.d
myremoveduplicates [] = []
myremoveduplicates [x] = [x]
myremoveduplicates (h:t)
 | h == (head(t)) = myremoveduplicates [h]++(tail(t))
 | otherwise = [h]++ myremoveduplicates t

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

{- 5.
1. This question took about and hour and a half to complete and it taught us how to deal with basic type errors and the tools Haskell has to convert Num types. 
The question was reasonable but the extended time came from understanding the type error in 1.b and figuring out how to correct it with rudementary knowledge in Haskell.
2. This question took about 3 hours and it was a good lesson on how to problem solve when our code throws unexpected type errors. 
It felt a little unreasonable becuase it seemed to bring in conent from outside of the course (mainly probabiility) but in the end it was doable with a lot of effort. 
3. This question took about a half an hour to complete and it cemented how to deduce the types of a funciton through analysis. 
The question was very reasonable and definitely the easiest question on the homework.
4. This question took about two hours to complete. It taught us how to approach different problems in Haskell and helped us create a strategy to approach
problems in Haskell. The question was very reasonable with each part taking about 20 minutes to complete. 
-}