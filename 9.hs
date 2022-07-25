module Main where

main :: IO ()
func :: Integer->Integer->Integer
func num res = if num < 0 then res else func (num-1) (res + num * 11 * 10 ^(num*2))
main = do
        putStrLn "enter value for x: "
        input1 <- getLine
        let x = read input1 :: Integer
        print(func x 0)