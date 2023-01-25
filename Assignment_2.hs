--Kallen Paterson (87052684)
--Alex Baillie (30018774)

-- Assignment 2


{-1a.
   i.   tails :: [a] -> [[a]]
   ii.  tails "happy" = [["happy"], ["appy"], ["ppy"] ["py"], ["y"]]
   iii. tails "happy" :: [[Char]]
   iv.   No it is not possible to define tails as a foldr or foldl without using recursion. This is because tails relies appending the whole list with each 
         subset of the list until it reaches the empty set. i.e each iteration has to go through tails.
  
  b.
   i.   doif :: (Bool -> a) -> (a -> a) -> [a] -> [a]
   ii.  doif even (‘div‘ 2) [11,22,33,44,55,66] = [11,11,33,22,55,33] :: [Int] -}
-- iii. 

doif f g [] = []
doif f g (h:t)
 | f h = g h : doif f g t
 | otherwise = h : doif f g t
 
toUpper :: Char -> Char
toUpper x = toEnum( fromEnum x - fromEnum 'a' + fromEnum 'A')
 
capVowel [] = []
capVowel (h:t) = doif (\ x -> x `elem` ['a','e','i','o','u']) toUpper (h:t)

-- iv.
doif1 f g [] = []
doif1 f g (h:t) = [if f x == True then g x else x | x <- (h:t)]

-- v.

doif2 f g [] = []
doif2 f g (h:t) = foldr (\x -> if f x then (g x:) else (x:)) [] (h:t) 
 
 
--2a.
harmonic 1 = 1
harmonic n = sum[ 1/(x) | x <- [1..n]]

-- b.
myremoveduplicates [] = []
myremoveduplicates (h:t) = foldr (\ x y -> if x `elem` y then y else (x:y)) [] (h:t)
 
--c.
myordered [] = True
myordered (h:t) 
 | length(t) == 0 = True
 | h <= head(t) = (True) && (myordered t)
 | otherwise  = False

-- d. (unfinished)

-- e. (unfinished)
myapply :: (Eq a ) => [a] -> [(a,a)] -> [a]
myapply [] (a:z) = []
myapply (h:t) (a:z) = [if x == fst(a) then y else x| x <- (h:t), y <-(map snd (a:z))]