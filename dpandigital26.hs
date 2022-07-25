module Main where

--We take an input such as 123. Find all the permutations of the tail of the input e.g [23,32]
--For each permutation, interleave the head (1) amongst each possible position:  [[123, 213, 231], [132, 312, 321]]
--Join the list of lists together by folding over it with ++ to return a list with all possible numbers
--the Interleave function takes 3 arguments e.g [] 1 [2,3] and returns the 3 args joined together [[1,2,3]]
--it recursively calls itself but with an element taken from right list & appended to left e.g [2] 1 [3] = [[2,1,3]]
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = foldr (++) [] (map (interleave [] x) (permutations xs)) 
                      where
                        interleave :: [a] -> a -> [a] -> [[a]]
                        interleave xs x [] = [xs ++ [x]]
                        interleave xs x (y:ys) = 
                            (xs ++ (x:y:ys)) : 
                            (interleave (xs ++ [y]) x ys)


--joins values of lists into 1 value because we have lists in lists in our case and we want to form numbers
-- e.g [[1,1,2,2],[1,2,1,2],[1,2,2,1],[1,1,2,2]] we get [1122,1212,1221,1122]
joiner :: [Integer] -> Integer
joiner = read . concatMap show

--the permutation function on list with duplicating values will produce repeating numbers
--So here we remove the duplicating numbers
remDup :: [Integer] -> [Integer]
remDup [] = []
remDup (x:xs) = x : (remDup(remove x xs))
    where
         remove :: Integer -> [Integer] -> [Integer]
         remove x [] = []
         remove x (y:ys) --in a recursive manner it checks for duplicating values
              | x==y = remove x ys
              | otherwise = y : (remove x ys)
    

main :: IO () --returns the number of dpandigitals that are divisible by n
main = do
         putStrLn "enter value for n: "
         input <- getLine
         let n = read input :: Integer
         let divisor = 10000000000000000000 -- if ans of div is 0 then it is removed bcoz 0 cant be in first position
         let dpandigital = [9,9,8,8,7,7,6,6,5,5,4,4,3,3,2,2,1,1,0,0] 
         print(length( filter(\x -> x `rem` n == 0 && x `div` divisor /= 0 ) (remDup (map joiner (permutations dpandigital))))) 
 



