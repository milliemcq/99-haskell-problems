-- P1 Find the last element in a list
lastElem xs = head (reverse xs)

-- P2 Find the last bit one element in a list
lastButOne xs = xs !! (length xs - 2)

-- P3 Find the Kth element of a list
elementAt xs i = xs !! i

-- P4 Find the number of elements in a list
myLength xs = length xs

-- P5 Reverse a list/ reverse without using reverse
newReverse [] = []
newReverse (x:xs) = newReverse xs ++ [x]

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
