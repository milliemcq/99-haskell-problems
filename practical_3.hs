-- Function eliminates duplicate elements of a list
pairsxs xs = zip xs (tail xs)
compress (x:xs) = x:[ y | (x, y) <- (pairsxs xs), x /= y]

-- Define pyths that maps an integers n to all such triples with components in [1..n]
pairsxs xs = zip xs (tail xs)

-- '<-' Means taking elements from this list
-- pyths = []
-- for x in range(1,n):
--   for y in range(1,n):
--     for z in range(1,n):
--       if x**2 + y**2 == z**2:
--         pyths.append((x,y,z))

pyths x = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


-- scalar product of two lists

scalar xs ys = [x | x <- xs, y <- ys]
