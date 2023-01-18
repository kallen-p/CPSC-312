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
--probOfWinning :: (Eq b, Num b) => b -> b -> b -> b
probOfWinning p s w 
 | s == w = p^w
 | otherwise = (binomCoef s w)*(p^w)*((1-p)^(s-w)) + (probOfWinning p (s-1) w)

fact 0 = 1
fact n = n* fact(n-1)

binomCoef n k
 | k > n = 0
 | otherwise = fact(n)/((fact(k))*(fact(n-k)))
 
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

{- 5.
1. This question took about and hour and a half to complete and it taught us how to deal with basic type errors and the tools Haskell has to convert Num types. 
The question was reasonable but the extended time came from understanding the type error in 1.b and figuring out how to correct it with rudementary knowledge in Haskell.
2. 
3. This question took about a half an hour to complete and it cemented how to deduce the types of a funciton through analysis. 
The question was very reasonable and definitely the easiest question on the homework.
4. This question took about two hours to complete. It taught us how to approach different problems in Haskell and helped us create a strategy to approach
problems in Haskell. The question was very reasonable with each part taking about 20 minutes to complete. 
-}