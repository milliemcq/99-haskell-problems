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
