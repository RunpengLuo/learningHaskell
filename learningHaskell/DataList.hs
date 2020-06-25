module DataList where

import Data.List as L


-- | intersperse
-- 
-- >>> intersperse "." "APPLE"
-- "A.P.P.L.E"
intersperse' :: a -> [a] -> [a]
intersperse' i ss = case ss of
    [] -> []
    [x]-> [x]
    x:xs -> x:i:(intersperse' i xs)


-- | intercalcate
-- 
-- >>> intercalcate  " " ["hey","there","guys"] 
-- "hey there guys"
intercalate' :: [a] -> [[a]] -> [a]
intercalate' i ss = case ss of
    [] -> []
    [x]-> x
    x:xs -> x ++ i ++ (intercalate' i xs)


-- | transpose
-- 
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]] 
-- [[1,4,7],[2,5,8],[3,6,9]]
transpose' :: [[a]] -> [[a]]
transpose' ss = case ss of
    []  -> []
    []:xs-> transpose' xs 
    _:_ -> (concat $ map t ss):(transpose' $ map d ss)
    where
        t = take 1
        d = drop 1


-- | iterate
iterates :: (a -> a) -> a -> [a]
iterates f x = x : (iterates f acc)
  where
      acc = f x

-- | group
groups :: Eq a => [a] -> [[a]]
groups [] = []
groups (x:xs) = (x : (takeWhile (x ==) xs)):(groups $ dropWhile (x ==) xs)


-- | span
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' f xs = (takeWhile f xs, dropWhile f xs)


-- | break
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f xs = (takeWhile (not.f) xs, dropWhile (not.f) xs)
-- | listToDic
-- 
-- >>> listToDic [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] 
-- [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
listToDic :: Ord a => [a] -> [(a,Int)]
listToDic = 
    map (\l@(x:_) -> (x,length l)) . group . sort


-- | search
-- Data.List: isInfixOf
search :: (Eq a) => [a] -> [a] -> Bool
search needle = 
    foldl (\acc x -> if take len x == needle then True else acc)  False . tails 
    where
        len = length needle

search' :: (Eq a) => [a] -> [a] -> Bool
search' needle = 
    foldr (\x acc -> if take len x == needle then True else acc) False . tails
    where
        len = length needle

-- | isPrefixOf
isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' needle = 
    foldr (\x acc -> if take len x == needle then True else acc) False . inits
    where
        len = length needle

-- | isSuffixOf
isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' needle =
    foldr (\x acc -> if (take len x == needle) && (len == length x) then True else acc) False . tails 
    where
        len = length needle

-- | partition
-- partition /= span !!!
partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' f list = (filter f list,filter (not.f) list)


-- | find
find' :: (a -> Bool) -> [a] -> Maybe a
find' f xs
  | null xs   = Nothing
  | otherwise = Just $ head $ filter f xs


-- | elemIndex
elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' x xs = Just $ length $ takeWhile (x /=) xs


-- | elemIndices
elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' needle list = foldr (\(x,i) acc -> if x == needle then i:acc else acc) [] indlists
    where
        indlists = zip list [0..pred $ length list]


-- | findIndex
findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' f xs = case ziplist of
    []  -> Nothing
    x:_ -> Just $ snd x 
    where
        ziplist = filter (\(x,_) -> f x) $ zip xs inds 
        inds = [0..pred $ length xs]


-- | findIndices
findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' f xs = snd $ unzip $ filter (\(x,_) -> f x) $ zip xs inds
    where 
        inds = [0..pred $ length xs] 


-- | lines 
-- 
-- >>> lines "first line\nsecond line\nthird line"
-- ["first line","second line","third line"]


-- | unlines
-- 
-- >>> unlines ["first line", "second line", "third line"]
-- "first line\nsecond line\nthird line\n"
unlines' :: [String] -> String
unlines' = foldr (\x acc -> x ++ "\n" ++ acc) ""


-- | words
-- 
-- >>> words "hey these are the words in this sentence"  
-- ["hey","these","are","the","words","in","this","sentence"] 


-- | unwords
-- 
-- >>> unwords ["hey","there","mate"] 
-- "hey there mate"
unwords' :: [String] -> String
unwords' = foldr (\x acc -> x ++ " " ++ acc) ""


-- | nub
-- 
-- >>> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
-- [1,2,3,4]
nub' :: Eq a => [a] -> [a]
nub' = foldl (\acc x -> x:delete' x acc) []


-- | delete
delete' :: Eq a => a -> [a] -> [a]
delete' i = filter (/= i)


-- | (\\)
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) ori del = case del of
    []   -> ori
    x:xs -> (\\\) (delete' x ori) xs


-- | union 
union' :: Eq a => [a] -> [a] -> [a]
union' a b = case b of 
    [] -> a
    x:xs | null $ filter (x==) a -> union' a xs ++ [x]
         | otherwise -> union' a xs 


-- | intersect
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' a b = case b of
    [] -> []
    x:xs | null $ filter (x ==) a -> intersect' a xs
         | otherwise -> x : intersect' a xs


-- Polymorphism functions:
-- genericLength
-- genericTake
-- genericDrop
-- genericSplitAt
-- genericIndex
-- genericReplicate

-- Generic functions:
-- nubBy
-- deleteBy
-- unionBy
-- intersectBy
-- groupBy
--
-- >>> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3] 
-- >>> groupBy (\x y -> (x > 0) == (y > 0)) values
-- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]] 


-- | on
-- 
-- >>> ((==) `on` (> 0)) 3 (-2)
-- False
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

