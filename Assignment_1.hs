

--1.a
{-harmonic :: (Eq t, Fractional t) => t -> t
harmonic takes n and returns 1+ 1/2 + .. + 1/n -}
--harmonic :: (Eq t, Fractional t) => t -> t
harmonic 1 = 1
harmonic n = (1/n) + harmonic (n-1)

--1.b 
{-didn't work with length[1,2,3,4] because the function assumes that the type that is given will be the type that is returned. -}


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
myapply :: [a] -> [(a,a1)] -> [a1]
--myapply [] (a:z) = []
myapply (h:t) (a:z)
 | h == fst(a) = snd(a) : myapply t (a:z)
 | otherwise = myapply [h] z : myapply t (a:z)
