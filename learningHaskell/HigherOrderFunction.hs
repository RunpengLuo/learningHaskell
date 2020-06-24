module HigherOrderFunction where


-- zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys
  | null xs || null ys = []
  | otherwise = 
      (f (head xs) (head ys)):(zipWith' f (tail xs) (tail ys))


-- flip
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


-- map
map' :: (a -> b) -> [a] -> [b]
map' f ss = case ss of
    []     -> []
    (x:xs) -> (f x):(map' f xs)


-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f ss = case ss of
    [] -> []
    (x:xs) | f x -> x : (filter' f xs)
           | otherwise -> filter' f xs



listOfFuns :: (Num a,Enum a) => [a -> a]
listOfFuns = map (*) [0..]


-- maximum
maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x acc -> max x acc)

-- scanl
sqrtSums :: Int  
sqrtSums = 
    succ $ length $ takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))

-- Function application `$`
-- f $ g $ x = f (g x)

-- Function composition `.`
-- f . g x = f (g x)
-- f . g . h x = f (g (h x))


-- sum v1
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- sum v2
sum'' :: (Num a) => [a] -> a
sum'' = foldl1 (+) 

-- odd square sum (less than 10000) v1
oddSquareSum :: Integer
oddSquareSum = 
    sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum' :: Integer
oddSquareSum' =
    let oddSquares  = filter odd $ map (^2) [1..]
        belowLimit  = takeWhile (<10000) oddSquares
    in sum belowLimit