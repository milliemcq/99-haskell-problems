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
packDupes (x:ys@(y:_))
    | x == y = packDupes [x, y] : ys
    | otherwise = x : packDupes ys
packDupes ys = ys 
