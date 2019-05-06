-- P1 Find the last element in a list
-- Iterates over tail until only one element remains
lastElem :: [a] -> a
lastElem [] = error "empty list"
lastElem [x] = x
lastElem (_:xs) = lastElem xs

-- P2 Find the last bit one element in a list
-- Eliminate the head of the list until only a head and a tail
-- with one element remains.
lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [_] = error "list too short"
lastButOne (x:[_]) = x
lastButOne (_:xs) = lastButOne xs

-- P3 Find the Kth element of a list
elemAt :: Int -> [a] -> a
elemAt _ [] = error "list too short"
elemAt 1 (x:_) = x
elemAt n (_:xs) = elemAt (n-1) xs

-- P4 Find the number of elements in a list
totalElem :: [a] -> Int
totalElem = foldr (\x -> (+)1) 0


-- P5 Reverse a list/ reverse without using reverse
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- P6 Find out whether a list is a palindrome
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome ([x,y]) = x == y
palindrome (x:xs) =
  (x == last xs) && palindrome (init xs)

-- P7 Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = foldr ((++).flatten) [] x

-- P8 Eliminate consecutive duplicates of list elements
eliminate :: Eq a => [a] -> [a]
eliminate [] = []
eliminate [x] = [x]
eliminate (x:y:xs) =
  if x == y
  then eliminate (x:xs)
  else x:eliminate (y:xs)

-- P9 Pack consecutive duplicates of list elements into sublists.
packHelper :: Eq a => a -> [[a]] -> [[a]]
packHelper a [] = [[a]]
packHelper a (x:xs) =
  if a == head x
  then (a:x):xs
  else [a]:x:xs

pack :: Eq a => [a] -> [[a]]
pack = foldr packHelper []

-- P10 Run-length encoding of a list
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)).pack

-- P12 Undo run length encoding
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

-- P14 Duplicate the elements of a list
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- P15 Replicate list elements a given number of times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- P16 Drop every Nth element of a list
dropEvery :: [a] -> Int -> [a]
dropEvery list count = helper list count count
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))

-- P17 Split a list into two parts, length of first part is given
split :: [a] -> Int -> ([a], [a])
split []         _             = ([], [])
split l@(x : xs) n | n > 0     = (x : ys, zs)
                   | otherwise = ([], l)
    where (ys,zs) = split xs (n - 1)

-- P18 extract a slice from a List
slice :: [a] -> Int -> Int -> [a]
slice [] _ _  = []
slice (x:xs) i k
 | i > 1      = slice xs (i - 1) (k - 1)
 | k < 1      = []
 | otherwise  = x:slice xs (i - 1) (k - 1)
