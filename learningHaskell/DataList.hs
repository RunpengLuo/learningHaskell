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
  | otherwise = Just (head $ filter f xs)


-- | elemIndex
elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' x xs = Just (length $ takeWhile (x /=) xs)


-- | elemIndices
elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' needle list = foldr (\(x,i) acc -> if x == needle then i:acc else acc) [] indlists
    where
        indlists = zip list [0..pred $ length list]


