

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
