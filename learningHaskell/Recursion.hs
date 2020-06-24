module Recursion where


-- maximum v1
maximum' :: Ord a => [a] -> a
maximum' list = case list of
    [] -> error "Empty List"
    [x]-> x
    x:xs | x > maxTail -> x
         | otherwise -> maxTail
         where
             maxTail = maximum' xs

--  maximum v2
maximum'' :: Ord a => [a] -> a
maximum'' list = case list of
    []   -> error "Empty List"
    [x]  -> x
    x:xs -> max x (maximum'' xs)


-- replicate v1
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  


-- take v1
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n list
  | n < 1 = []
  | otherwise = case list of
      []    -> []
      (x:xs)-> x:(take' (n - 1) xs)


-- reverse v1 O(n^2)
reverse' :: [a] -> [a]
reverse' list = case list of
    []    -> []
    (x:xs)-> (reverse' xs) ++ [x]

-- reverse v2 O(n)
reverse'' :: [a] -> [a]
reverse'' = helper []
  where
      helper acc list = case list of
          []    -> acc
          (x:xs)-> helper (x:acc) xs 


-- repeat  
repeat' :: a -> [a]
repeat' x = x:repeat' x


-- zip
zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys
  | null xs || null ys = []
  | otherwise = 
      (head xs, head ys):zip' (tail xs) (tail ys)


-- elem
elem' :: Eq a => a -> [a] -> Bool
elem' x ss = case ss of
    [] -> False
    (y:ys) | y == x    -> True
           | otherwise -> x `elem'` ys


-- sort

-- Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
      insert :: Ord a => a -> [a] -> [a]
      insert x ss = case ss of
          [] -> [x]
          y:ys | x <= y -> x : y : ys
               | otherwise -> y : insert x ys

-- Quick Sort v1
quickSort :: Ord a => [a] -> [a]
quickSort ss = case ss of
    []   -> []
    (_:_) -> smallerSorted ++ [x] ++ biggerSorted
    where
        smallerSorted = [a | a <- xs, a <= x]
        biggerSorted  = [a | a <- xs, a >= x]
        xs = tail ss
        x  = head ss

-- Quick Sort v2
quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = 
    let 
        smallerSorted = [a | a <- xs, a <= x]
        biggerSorted  = [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- Quick Sort v3
quickSort'' :: Ord a => [a] -> [a]
quickSort'' [] = []
quickSort'' (x:xs) =
    let smallerSorted = quickSort'' (filter (<=x) xs)
        biggerSorted  = quickSort'' (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted