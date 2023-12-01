

import System.IO
import Data.Char
import Distribution.Simple.Program.HcPkg (list)



----------------------------------------------------part 1: solution------------------------------------------------------

main :: IO ()
main = do
    --input parsing
  fileHandle <- openFile "input.txt" ReadMode
  contents <- hGetContents fileHandle
  inputList <- return $ splitNewline contents
  --solution
  print inputList
  result <- return (sum (inputToNumbers inputList))
  print result
    --input parsing
  hClose fileHandle


firstNumber :: String->Int
firstNumber (x:xs)
 |isDigit x = (read [x])
 |otherwise =firstNumber xs

lastNumber :: String->Int
lastNumber = firstNumber . reverse

wholeNumber :: String->Int
wholeNumber s = 10*(firstNumber s)+(lastNumber s)


inputToNumbers :: [String]->[Int]
inputToNumbers input= map wholeNumber input










--------------------------------------------part 2: electric boogaloo----------------------------------
main2 :: IO ()
main2 = do
    --input parsing
  fileHandle <- openFile "input.txt" ReadMode
  contents <- hGetContents fileHandle
  inputList <- return $ splitNewline contents
    --solution
  result <- return (sum (inputToNumbers2 inputList))
  print result
    --input parsing
  hClose fileHandle



strNumber::String->Int
strNumber "one"=1
strNumber "two"=2
strNumber "three"=3
strNumber "four"=4
strNumber "five"=5
strNumber "six"=6
strNumber "seven"=7
strNumber "eight"=8
strNumber "nine"=9
strNumber s= strNumber $ reverse s

startsWith :: String->String->Bool
startsWith big start 
 | (length big)<(length start)=False
 |otherwise = all (sameOnIndex big start) [0..(length start)-1]
 
startsWithList :: [String]->String->Bool
startsWithList list big = any (startsWith big)  list
startsWithWhat :: [String]->String->String
startsWithWhat list big = head $ filter (startsWith big) list



sameOnIndex::String->String->Int->Bool
sameOnIndex big start index = (big !! index) == (start !! index)

firstNumber2 :: [String]->String->Int
firstNumber2 numberlist (x:xs)
 |isDigit x = (read [x])
 |startsWithList numberlist (x:xs)   = strNumber (startsWithWhat numberlist (x:xs))
 |otherwise =firstNumber2 numberlist xs
--["one","two","three","four","five","six","seven","eight","nine"]
lastNumber2 :: [String]->String->Int
lastNumber2 numlist str = firstNumber2 (map reverse numlist) (reverse str)

wholeNumber2 :: [String]->String->Int
wholeNumber2 list str = 10*(firstNumber2 list str)+(lastNumber2 list str)

inputToNumbers2 :: [String]->[Int]
inputToNumbers2 input= map (wholeNumber2 ["one","two","three","four","five","six","seven","eight","nine"]) input
 ----------------------------------------------------input parsing----------------------------------------------------

splitNewline :: String->[String]
splitNewline str =splitNewlineAcc str "" 

splitNewlineAcc:: String->String->[String]
splitNewlineAcc "" "" = []
splitNewlineAcc "" acc = [acc]
splitNewlineAcc (ch:rest) acc 
 |ch == '\n' = [acc] ++ splitNewlineAcc rest ""
 |otherwise = splitNewlineAcc rest (acc++[ch])




