{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

maxFileSize::Int
maxFileSize = 12354861385


data File




---i am too lazy to parse input today, luckily it is a good day to be too lazy for input parsing.
input ::[(Int,Int)]
input = [(40,277),(82,1338),(91,1349),(66,1063)]

testInput ::[(Int,Int)]
testInput = [(7,9),(15,40),(30,200)]


----------------------part1-----------------------
solve :: [(Int,Int)]-> Int
solve l = foldr (*) 1 (map winning_distances l)

winning_distances :: (Int,Int)->Int
winning_distances (time,distance) = length $ filter (>distance) (possible_distances time)

possible_distances :: Int->[Int]
possible_distances time = map (distance time) [0..time]
 where 
    distance::Int->Int->Int
    distance maxtime pushtime = pushtime*(maxtime-pushtime)





------------part 2------------
solvePart2 :: Int
solvePart2 = winning_distances $ getBigTuple input

getBigTuple :: [(Int,Int)]->(Int,Int)
getBigTuple input = (getBigNumber (map fst input),getBigNumber (map snd input))

getBigNumber :: [Int]->Int
getBigNumber input = read (foldr f "" input)
 where f::Int->String->String
       f i s = show i ++ s

