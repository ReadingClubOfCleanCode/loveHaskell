import Data.List

descSort :: [Int] -> [Int]
descSort = sortBy (\x y -> compare y x)

getCoinCount :: [Int] -> Int -> Int
getCoinCount _ 0 = 0
getCoinCount [] _ = 0
getCoinCount (x:xs) a 
	| x <= a = getCoinCount xs (a - x) + 1
	| otherwise = getCoinCount (xs) a

