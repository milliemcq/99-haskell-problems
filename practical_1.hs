n = a `div` length xs
    where
       a = 10
       xs = [1,2,3,4,5]

--returns the last element in a list
lastNum xs = head (reverse xs)

lastNumDifferent xs = xs !! (length xs)

--removes the last element in a list
removeLast xs = reverse (drop 1 (reverse xs))

removeOtherLast xs = take (length xs - 1) xs

--remove the first element and append it to the end
shuffle [] = []
shuffle xs = (tail xs) ++ [head xs]
