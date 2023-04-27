import Data.Char
import Data.Maybe

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

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show
headCL (CUnit a) = a
headCL (Consnoc p u x) = p

insert EmptyCL d = (CUnit d)
insert (CUnit a) d = (Consnoc a EmptyCL d)
insert (Consnoc a b c) d = (Consnoc a (insert b c) d) 

tailCL (CUnit a) = EmptyCL
tailCL (Consnoc p u x) = (insert u x)

isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit (CUnit x) = True
isCUnit _ = False

lenCL EmptyCL = 0
lenCL (CUnit x) = 1
lenCL (Consnoc a u b) = 2+ (lenCL u)

cl = Consnoc 2 (Consnoc 3 (CUnit 4) 5) 6

--reverseCL EmptyCL = EmptyCL
--reverseCL (CUnit a) = (CUnit a)
reverseCL (Consnoc a u b) = (Consnoc b u a)
reverseCL x = x

-- se va ala c_je_a de su madre el CL.


data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show


eval(Lit x) = x
eval(Add x y ) = (eval x)+ (eval y)
eval(Sub x y ) = (eval x) - (eval y)
eval(Prod x y) = (eval x) * (eval y)
eval(Div x y) = div (eval x)  (eval y)


pepe = Div (Add (Lit 10) (Lit 2)) (Prod (Lit 2) (Lit 3) )

constructExp :: String -> [Exp] -> Exp
constructExp ('+':xs) stack = constructExp xs ((Add  (head (tail stack)) (head stack)):(tail (tail stack)))
constructExp ('*':xs) stack = constructExp xs ((Prod (head (tail stack)) (head stack)):(tail (tail stack)))
constructExp ('/':xs) stack = constructExp xs ((Div  (head (tail stack)) (head stack)):(tail (tail stack)))
constructExp ('-':xs) stack = constructExp xs ((Sub  (head (tail stack)) (head stack)):(tail (tail stack)))
constructExp (' ':xs) stack = constructExp xs stack
constructExp (x:xs)   stack = constructExp xs ((Lit (digitToInt x)):stack)
constructExp []       stack = (head stack)

parseRPN rpn = constructExp rpn []

evalRPN rpn = eval (parseRPN rpn)

pe = "8 5 3 - 3 * +"

-- 6 a) los maneja mal.


seval(Lit x) = x
seval(Add x y ) = (eval x)+ (eval y)
seval(Sub x y ) = (eval x) - (eval y)

seval(Prod x y ) = let a = seval(x)
                       b = seval(y)
					in if a == Nothing || b == Nothing then Nothing else ( Just ( (fromMaybe 1 a)*(fromMaybe 1 b)) )

seval(Div x y) = let den = (eval y) in if den == 0 then Nothing else  (Just div (eval x) den)