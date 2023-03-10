--Kallen Paterson (87052684)
--Alex Baillie (30018774)

-- Assignment 3
--1.a:

module TwentyQs where

-- To run it, try:
-- ghci
-- :load TwentyQs
-- go

import System.IO

data QATree = QLeaf String
            | QNode String QATree QATree
       deriving (Show)

initQATree = QNode "Is it living?"
                (QNode "Is it a person?"
                    (QLeaf "Justin Bieber")
                    (QLeaf "Tahlequah (J-35), a southern resident killer whale"))
                (QNode "Is it a physical object?"
                    (QLeaf "Whistler")
                    (QLeaf "CPSC 312"))

play :: QATree -> IO QATree
play tree =
   do
      putStrLn "Do you want to play 20 questions?"
      ans <- getLine
      if (ans `elem` ["y","yes","ye","oui"])
        then do
           putStrLn "Think of an entity"
           newtree <- askabout tree
           play newtree
        else return tree

askabout :: QATree -> IO QATree
askabout (QLeaf ans) =
  do
    putStrLn("Is it "++ans++"?")
    line <- getLine
    if (line `elem` ["y","yes","ye","oui"])
       then return (QLeaf ans)
       else do
          --putStrLn("I'm sorry, please try again")
			addnewq (QLeaf ans)
          --return (QLeaf ans)
          
askabout (QNode q yes no) =
  do
    putStrLn(q)
    line <- getLine
    if (line `elem` ["y","yes","ye","oui"])
       then do
            newyes <- askabout yes
            return (QNode q newyes no)
       else do
            newno <- askabout no
            return (QNode q yes newno)
     

addnewq :: QATree -> IO QATree
addnewq (QLeaf ans) = 
  do
    putStrLn "What are you thinking of?" 
    obj <- getLine 
    if (obj == "")
	    then return (QLeaf ans)
        else do
	         putStrLn ("Give a question for which the answer is yes for "++obj++" and no for "++ans) 
	         quest <- getLine 
	         return (QNode quest (QLeaf ans) (QLeaf obj))
	
go :: IO QATree
go = play initQATree

--1.b
fixdel :: String -> String

fixdel (w:x:y:z:t)  
 | length (w:x:y:z:t) <= 3 = (w:x:y:z:t)
 | [w] ++ [x] ++ [y] ++ [z] == "\DEL" = fixdel t
 | otherwise = [w] ++ fixdel (x:y:z:t)

fixdel _ = []

 --3.a
splitsep :: (a-> Bool) -> [a] -> [[a]]
splitsep param (h:t)
 | param h = splitsep param t
 | otherwise = let(y,z) = break param t
               in (h:y) : splitsep param z
 
splitsep _ [] = []
