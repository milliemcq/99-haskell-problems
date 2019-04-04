-- 4.1: Halve a list splitting even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve ls = x !! (length ls/2)

-- Define a function third that returns the third element in a list
third :: [a] -> Int
