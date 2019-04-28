mystery1 = 1 : map (*2) mystery1
mystery2 = 1 : map (*2) mystery1

mapTest = 1 : map (+1) mapTest

my_const c x = c
my_append [] ys = ys
my_append (x:xs) ys = x : my_append xs ys
