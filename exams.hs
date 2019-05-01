mystery1 = 1 : map (*2) mystery1
mystery2 = 1 : map (*2) mystery1

mapTest = 1 : map (+1) mapTest

my_const c x = c

my_append [] ys = ys
my_append (x:xs) ys = x : my_append xs ys

my_map f [] = []
my_map f (x:xs) = f x : my_map f xs


dwindle :: Eq a => [a] -> [a]
dwindle [] = []
dwindle [x] = [x]
dwindle (x:xs@(y:ys))
  | x == y    = [x] ++ dwindle ys
  | otherwise   = [x, y] ++ dwindle ys
