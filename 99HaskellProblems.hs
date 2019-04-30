-- P1 Find the last element in a list
lastElem :: [a] -> a
lastElem [] = error "empty list"
lastElem [x] = x
lastElem (_:xs) = last xs

-- P2 Find the last bit one element in a list
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
