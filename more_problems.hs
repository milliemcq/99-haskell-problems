-- Quicksort (doesn't work)
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, x <= x]
                  larger = [b | b <- xs, b > x]

--My First Script
double x = x + x

quadruple x = double(double x)
