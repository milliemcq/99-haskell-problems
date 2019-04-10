-- 4.1: Halve a list splitting even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- 4.2: Define a function third that returns the third element in a list
third :: [a] -> a

third xs = xs !! 2

-- 4.3: Safetail maps empty list to itself rather than error
safetail :: [a] -> [a]
safetail ([]:[]) = []
safetail (_:xs) = xs

-- 4.4 Show how the disjunction operator || can be defined in 4 different ways using pattern matching
(||) :: Bool -> Bool -> Bool

-- Ch 6 Recursion
init (x:xs) | null xs = []
            | otherwise =
