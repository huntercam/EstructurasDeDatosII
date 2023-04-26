data Color = P Float Float Float deriving Show

pm a b = ( (a+b)/ 2)
mezclar (P a b c) (P e f g) = ( P (pm a e) (pm b f) (pm c g) )

data Linea = W String Int deriving Show

vacia = W [] 0
moverIzq (W x 0) = (W x 0)
moverIzq (W x n) = (W x (n-1) )
moverDer (W x n) | n < (length x) = (W x (n+1) )
                           | True = (W x n)
moverIni (W x n) = (W x 0)
moverFin (W x n) = (W x (length x)) 
insertar c (W x n) = (W ((take n x) ++ [c] ++ (drop n x)) (n+1))
borrar (W x 0) = (W x 0)
borrar (W x n) = (W ((take (n-1) x) ++ (drop n x)) (n-1))

