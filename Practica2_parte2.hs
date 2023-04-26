azip3 _ _ [] = []
azip3 _ [] _ = []
azip3 [] _ _ = []
azip3 xs ys zs = let m = zip (zip xs ys) zs in map (\x -> (fst( fst(x) ), snd( fst(x) ), snd(x)) ) m


type NumBin = [Bool]

sumaaux [] [] False = []
sumaaux [] [] True = [True]
sumaaux [] ys c = sumaaux [False] ys c
sumaaux xs [] c = sumaaux xs [False] c
sumaaux (x:xs) (y:ys) c = if x && y then c:(sumaaux xs ys True) else if (x || y) && c then False:(sumaaux xs ys True) else ((x || y) || c):(sumaaux xs ys False)

suma xs ys = sumaaux xs ys False

corrimiento xs 0 = xs
corrimiento xs n = False:(corrimiento xs (n-1))

prodaux xs (idx,y) = if y then corrimiento xs idx else []

prod xs ys = foldr suma [] (map (\x -> prodaux xs x) (zip [0..] ys))

divisors x = [y | y <- [1..x] , (mod x y) == 0 ]

matches x ys = [y | y <- ys, y == x]

cuadruplas n  = [ (a,b,c,d) | a <- [1..n] , b <- [1..n], c <- [1..n], d <- [1..n], (a*a + b*b) == (c*c+d*d)  ]

unique xs = [x | (i,x) <- (zip [0..] xs), (length [y | (_,y) <- (zip [0..i-1] xs), y == x])  == 0 ]


scalarproduct xs ys = sum [p*q | (p, q) <- (zip xs ys)]

