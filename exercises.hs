-- 4.1: Halve a list splitting even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Define a function third that returns the third element in a list
third :: [a] -> a

third xs = xs !! 3
