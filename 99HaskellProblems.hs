-- P1 Find the last element in a list
lastElem xs = head (reverse xs)

-- P2 Find the last bit one element in a list
lastButOne xs = xs !! (length xs - 2)

-- P3 Find the Kth element of a list
elementAt xs i = xs !! i

-- P4 Find the number of elements in a list
myLength xs = length xs

-- P5 Reverse a list/ reverse without using reverse
myReverse xs = reverse xs

myReverseWoReverse [] = []
myReverseWoReverse (x:xs) = myReverseWoReverse xs ++ [x]

-- P6 Find out whether a list is a palindrome
isPalindrome xs = xs == reverse xs

-- P7 Flatten a nested list structure - HARD
-- data NestedList a = Elem a | List [NestedList a]
-- flatten :: NestedList a -> [a]
-- flatten (Elem x) = [x]
-- flatten (List x) = concatMap flatten x

-- P8 Eliminate consecutive duplicates of list elements
-- The @ symbol stands for AS, so it splits the tail into head and tail
-- Enabling to use x and y as consequtive elements
elimDupes (x:ys@(y:_))
    | x == y    = elimDupes ys
    | otherwise = x : elimDupes ys
elimDupes ys = ys

-- P9 Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : pack rest
         where
           getReps [] = ([], [])
           getReps (y:ys)
                   | y == x = let (f,r) = getReps ys in (y:f, r)
                   | otherwise = ([], (y:ys))
           (first,rest) = getReps xs

-- RACE FROM THE TOP

-- P1 Find the last element in a list
myLast xs = head (reverse xs)

-- P2 Find the last but on element of a list
myLastB xs = xs !! (length xs - 2)

-- P3 Find Kth Element in a List
elemAt xs k = xs !! (k-1)

-- P4 Find the number of elements in a list (without using length)
lengthList [] = 0
lengthList (_:xs) = 1 + lengthList xs

-- P5 reverse a list
newReverse [] = []
newReverse (x:xs) = newReverse xs ++ [x]
