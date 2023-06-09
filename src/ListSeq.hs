module ListSeq where

import Seq
import Par

--newtype ListSeq a = NIL | cons a (ListSeq a)  deriving Show

reducetree:: (a -> a -> a) -> TreeView a [a] -> a
reducetree f (ELT x) = x
reducetree f (NODE ls rs) = let (ca,cb) =  (reducetree f (showtS ls)) ||| (reducetree f (showtS rs))
                            in  (f ca cb)

expandir:: (a -> a -> a) -> [a] -> [a] -> [a]
expandir f (fx:fxs) (x:y:xs) = fx:((f fx x):(expandir f fxs xs))
expandir f [fx] [x] = [fx]
expandir f [] [] = []

--expandir:: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
--expandir f comprimido ls i n | i >= n       = []
--                             | i `mod` 2 == 0 = (head comprimido):(expandir f comprimido ls (i+1) n)
--                             | i `mod` 2 == 1 = (f (head comprimido) (head ls)):(expandir f (tail comprimido) (drop 2 ls) (i+1) n)

emparejar:: (a -> a -> a) -> [a] -> [a]
emparejar f (x:y:xs) = (f x y):(emparejar f xs)
emparejar f [x] = [x]
emparejar f _ = []

-- version paralela de tabulate
mapTree f EMPTY = []
mapTree f (ELT a) = [f a]
mapTree f (NODE ls rs) = let
    (izq, der) = (mapTree f (showtS ls)) ||| (mapTree f (showtS rs))
    in izq ++ der

instance Seq [] where
    emptyS = []
    singletonS a = [a]
    lengthS ls = (length ls)
    nthS ls i = ls !! i
    tabulateS f n = map f [0..(n-1)]
    mapS f ls = map f ls
    filterS f ls = filter f ls
    appendS ls lb = ls ++ lb
    takeS ls n = take n ls
    dropS ls n = drop n ls
    --- es trabajo N log N.  (por take y drop) : ¿Entonces que onda, esta bien?
    showtS ls = let size = lengthS ls in
               case size of
                  0 -> EMPTY
                  1 -> ELT (head ls)
                  _ -> let pot2 =  2 ^ floor ( logBase (fromIntegral 2) (fromIntegral (size-1)) )
                           (izq, der) = (take pot2 ls) ||| (drop pot2 ls)
                       in (NODE izq der)
    
    showlS [] = NIL
    showlS (l:ls) = CONS l ls

    joinS [emptyS] = emptyS
    joinS (l:ls) = appendS l (joinS ls)
    --reducetree f EMPTY no puede pasar
    reduceS f neutro [] = neutro
    reduceS f neutro ls = (f neutro (reducetree f (showtS ls)) ) -- como no es vacia, no hay emptys
  
    --- si no es asociativa que onda?
    --- convertiormos el problema a la mitad de chico,
    --- resolvememos ese problema. y con eso, resolvemos el grande.



    scanS f neutro []  = ( [] , neutro )
    scanS f neutro [a] = ([neutro], f neutro a)
    scanS f neutro ls = let reduccion = (emparejar f ls) --- [x,y]
                            (recursion, total) = scanS f neutro reduccion -- (<b, b*a1*a2>, b*a1*a2*a3*a4>)
                            expansion =  expandir f recursion ls
                       in (expansion, total)
                       
    fromList xs = xs

 --  (a,b,c,d) -> (ab,cd) -> (abcd)
 --   ([neutro, neutroa, neutroab, neutro abc], neutroabcd)             ([neutro, neutroab] , neutroabcd)    ( [neutro] , neutroabcd)

 --  (a,b,c,d,e) -> (ab, cd, e) -> (abcd, e) -> (abcde)
 --                     ([neutro, neutroab, neutroabcd], neutroabcde) -> ([neutro,neutroabcd], neutroabcde)   ->  ([neutro], neutroabcde)
  

--- se testea con
--- cabal update
--- cabal install --lib HUnit
--- cargamos listTests -> corremos main.