


import System.IO
import Data.Char
import Distribution.Simple.Program.HcPkg (list)



----------------------------------------------------part 1: solution------------------------------------------------------

main :: IO ()
main = do
    --input parsing
  fileHandle <- openFile "input.txt" ReadMode
  contents <- hGetContents fileHandle
  inputList <- return $ string_to_words contents
    --solution

  print inputList

  result <- return (solve inputList)
  print result
    --input parsing
  hClose fileHandle


reduce_list :: [Int]->[Int]
reduce_list (x:y:[]) = [  (y-x)]
reduce_list (x:y:xs) = [  (y-x)] ++ (reduce_list $ y:xs)

lists_until_zero :: [Int]->[[Int]]
lists_until_zero l 
 |all (==0) l = [l]
 |otherwise   = l : (lists_until_zero (reduce_list l))

append_number :: [[Int]]->Int
append_number l = foldr f 0 l
 where f :: [Int]->Int->Int
       f l i = (last l)+i

result :: [Int]->Int
result = append_number . lists_until_zero

solve :: [[Int]]->Int
solve input = sum (map result input)
-- fill_lists number [x] = [x]
-- fill_lists number lists = fill_lists (number + (last $ last $ lists)) (init lists)




--------------------------------------------part 2: electric boogaloo----------------------------------




 ----------------------------------------------------input parsing----------------------------------------------------

splitNewline :: String->[String]
splitNewline str =splitNewlineAcc str "" 

splitNewlineAcc:: String->String->[String]
splitNewlineAcc "" "" = []
splitNewlineAcc "" acc = [acc]
splitNewlineAcc (ch:rest) acc 
 |ch == '\n' = [acc] ++ splitNewlineAcc rest ""
 |otherwise = splitNewlineAcc rest (acc++[ch])


splitSpace :: String->[String]
splitSpace str =splitNewlineAcc str "" 

splitSpaceAcc:: String->String->[String]
splitSpaceAcc "" "" = []
splitSpaceAcc "" acc = [acc]
splitSpaceAcc (ch:rest) acc 
 |ch == '\n' = [acc] ++ splitSpaceAcc rest ""
 |otherwise = splitSpaceAcc rest (acc++[ch])




string_to_words :: String->[[Int]]
string_to_words s = map (map read) (map words (lines s))






ip::[[Int]]
 
ip = [[0,3,6,9,12,15],[1,3,6,10,15,21],[10,13,16,21,30,45]] 

