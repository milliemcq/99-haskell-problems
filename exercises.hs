-- 4.1: Halve a list splitting even-lengthed list into two halves.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- 4.2: Define a function third that returns the third element in a list
third :: [a] -> a

third xs = xs !! 2

-- 4.3: Safetail maps empty list to itself rather than error
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- 4.4 Show how the disjunction operator || can be defined in 4 different ways using pattern matching
(||) :: Bool -> Bool -> Bool
True || _ = True
_ || _ = False

-- 4.5 AND using nested conditions
my_and :: Bool -> Bool -> Bool
my_and x y = if x == True then (if y == True then True else False) else False

-- 4.6 AND using nested conditions differently
my_and_diff :: Bool -> Bool -> Bool
my_and_diff x y = if x == True then y else False

-- 4.7 show the meaning of curried function can be formalised in terms of lambda expressions
{-
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z
-}

multLambda :: Int -> (Int -> (Int -> Int))
multLambda = (\x -> \y -> \z -> x*y*z)

-- 4.8 Define a function luhnDouble that doubles a digit and subtracts 9 if result is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = if (x*2) > 9 then ((x*2) - 9) else (x*2)

-- 4.9 Define a function luhn that decides if a 4 digit bank card number is valid
luhn :: Int -> Int -> Int -> Int -> Bool
luhn u x y z = if ((luhnDouble x + luhnDouble y + luhnDouble z) `mod` 10 == 0) then True else False

-- 5.1 Give an expression that calculates the sum of 1^2 + 2^2 ... + 100^2 using list comprehension
calculateXSquare :: Int
calculateXSquare = [x^2 | x <- [1..100]]


-- Ch 6 Recursion
{-init (x:xs) | null xs = []
            | otherwise =-}
