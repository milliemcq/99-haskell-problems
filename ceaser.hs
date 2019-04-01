-- A ceaser cipher and decoder in Haskell

let2int :: Char -> Int
let2int = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
