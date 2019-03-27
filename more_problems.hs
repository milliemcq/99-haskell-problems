-- Quicksort
 qsort [] = []
 qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, x <= x]
                  larger = [b | b <- xs, b > x]

-- Quicksort in reverse
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, x >= x]
                 larger = [b | b <- xs, b < x]
