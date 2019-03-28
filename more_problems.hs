-- Quicksort (doesn't work)
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, x <= x]
                  larger = [b | b <- xs, b > x]

--My First Script
double x = x + x

quadruple x = double(double x)

--Broken to be fixed
funcN = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- Function to select last elem
my_last xs = xs !! (length xs - 1)

my_last_reverse xs = head (reverse xs)

-- Remove last element from non-empty list
my_init_reverse xs = reverse( tail (reverse xs))
