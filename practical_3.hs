-- Function eliminates duplicate elements of a list
pairsxs xs = zip xs (tail xs)
compress (x:xs) = x:[ y | (x, y) <- (pairsxs xs), x /= y]

-- Define pyths that maps an integers n to all such triples with components in [1..n]
pairsxs xs = zip xs (tail xs)

-- '<-' Means taking elements from this list
pyths x = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

pyths n = [(x, y, z) | (x, y, z) <- (list), x^2 + y^2 == z^2]
