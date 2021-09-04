msort :: Ord a => [a] -> [a]
msort xs = if length xs <= 1 then
                   xs
               else
                   merge (msort ls) (msort rs)
                   where
                       ls = take (div (length xs) 2) xs
                       rs = drop (div (length xs) 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge b [] = b
merge (x:xs) (y:ys) = if x <= y then
                          [x] ++ merge xs (y:ys)
                      else
                          [y] ++ merge (x:xs) ys

belongs :: Eq a => a -> [a] -> Bool
belongs y [] = False
belongs y (x:xs) = if y == x then True
                else esta y xs

seleccionar :: [a] -> Int -> a
seleccionar (x:_) 0 = x
seleccionar (_:xs) n = seleccionar xs (n-1)

replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar x y = [y] ++ replicar (x-1) y

concatt :: [[a]] -> [a]
concatt [] = []
concatt (xs:xss) = xs ++ concatt xss

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion [True] = True
conjuncion [False] = False
conjuncion (x:xs) = if x == False then conjuncion [x]
                    else conjuncion xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
                   where
                       smaller = [a | a <- xs, a <= x]
                       larger  = [b | b <- xs, b > x]

zippo :: [a] -> [b] -> [(a,b)]
zippo _ [] = []
zippo [] _ = []
zippo (x:xs) (y:ys) = (x, y) : zippo xs ys

producto :: Num a => [a] -> a
producto [] = 1
producto (x:xs) = x * producto xs

len :: [a] -> Int
len [] = 0
len (_: xs) = 1 + len xs

revertir :: [a] -> [a]
revertir [] = []
revertir (x:xs) = revertir xs ++ [x]
