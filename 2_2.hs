import Data.List

descSort :: [Int] -> [Int]
descSort = sortBy (\x y -> compare y x)

-- How do I use descSort result?
-- I want to use descSort result to getCoinCount.
getCoinCount :: [Int] -> Int -> Int
getCoinCount _ 0 = 0
getCoinCount [] _ = 0
getCoinCount (x:xs) a 
	| x <= a = getCoinCount xs (a - x) + 1
	| otherwise = getCoinCount (xs) a

