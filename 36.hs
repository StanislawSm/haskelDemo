module Main where

main :: IO ()
main = do
  print "Type p from 0 to 0.99:" 
  input <- getLine					-- geting input from user
  let p = read input :: Double      -- casting input to double
  putStrLn "Your answer is: "
  findP 0 0 p  						-- caling findP method with mon and non equal 0

-- isIncreasing and isDecreasing are getting arrays as arguments, so they can iterate through singe digits

isIncreasing :: (Ord a) => [a] -> Bool  -- this method checks if a number is increasing, so monotonic
isIncreasing [] = True					-- for an empty array true
isIncreasing [x] = True					-- for an one digit array also true
isIncreasing (x:y:xs) = x <= y && isIncreasing (y:xs)  -- dividing an array for x, y and a rest. Then checking if x <= y, and if before the array was increasing, if both then true

isDecreasing :: (Ord a) => [a] -> Bool -- same as isIncreasing, but contrarily
isDecreasing [] = True
isDecreasing [x] = True
isDecreasing (x:y:xs) = x >= y && isDecreasing (y:xs)


findP :: (Show a, RealFrac a) => a -> a -> a -> IO ()  -- argumenst have to be comparatable and divisable. Result of this method is to print a result
findP 0 0 p = findP 1 0 p  							   -- If mon and non are both 0 we increment mon, beceause one digit number is monotonic
findP mon non p = 
       if non / (mon + non) < p						   -- calculating current proportion mon to non and if its to low we add next number
	   -- here magic happens. We have to check if a next number is monotonic, so increasing or decreasing. But to do call findP we have to cast before a number to string, beceause findP takes an array of digits.
	   -- its performed by show function. Round function is required beceause mon and non are like doubles or floats. To cast them to string we have to round them, for example to Int
        then if isIncreasing (show (round (mon + non + 1) :: Int)) || isDecreasing (show (round (mon + non + 1) :: Int)) 
              then findP (mon + 1) non p  					-- if next number is mon we add one to amount of monotinic numbers            
              else findP mon (non + 1) p 				    -- else we add one non monotonic number
        else print (round (mon + non) :: Int)			    -- if our current proportion mon to non is bigger or equal to that given one we just calculate and print a result
      