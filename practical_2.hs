swap (x, y) = (y, x)
pair x y = (x, y)
double x = x * x
palindrome xs = xs == reverse xs
twice f x = f (f x)

myButLast xs =  xs !! (length xs - 2)

take17 xs = (xs !! 0, xs !! 6)

safetailConditional xs = if ((length xs) == 0) then [] else tail xs

safetailGuard xs |(length xs == 0) = []
                 | otherwise = tail xs

safetailPatternMatching :: [a] -> [a]
safetailPatternMatching [] = []
safetailPatternMatching xs = tail xs
