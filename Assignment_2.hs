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
