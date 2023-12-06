{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


---i am too lazy to parse input today, luckily it is a good day to be too lazy for input parsing.
input ::[(Int,Int)]
input = [(40,277),(82,1338),(91,1349),(66,1063)]

testInput ::[(Int,Int)]
testInput = [(7,9),(15,40),(30,200)]


solve :: [(Int,Int)]-> Int
solve l = foldr (*) 1 (map winning_distances l)

winning_distances :: (Int,Int)->Int
winning_distances (time,distance) = length $ filter (>distance) (possible_distances time)

possible_distances :: Int->[Int]
possible_distances time = map (distance time) [0..time]
 where 
    distance::Int->Int->Int
    distance maxtime pushtime = pushtime*(maxtime-pushtime)



