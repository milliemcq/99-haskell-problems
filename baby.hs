-- Starting out
-- :set prompt "ghci> "
-- :l baby


doubleMe x = x + x
-- doubleUs x y = x + x + y + y
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2
-- Functions can't begin with uppercase letters


--creating mathematical set definitions is possible
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

-- let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- Check if number supplied is 7 or not
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- When making patterns always make sure you inclue a catch-all pattern
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
