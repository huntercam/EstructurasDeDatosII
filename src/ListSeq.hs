{- Implementación del TAD secuencia -}

import Seq
import Par


--newtype ListSeq a = NIL | cons a (ListSeq a)  deriving Show

type ListSeq a = [a]

reducetree f ELT x = x
reducetree f NODE a b = let (ca,cb) =  (reducetree a) ||| (reducetree b)
                        in  f ca cb  
expandir recursion ls i n | i >= n-1       = []
                          | i `mod` 2 == 0 = (hd recursion):(expandir recursion ls (i+1) n)
                          | i `mod` 2 == 1 = (f (hd recursion) (hd ls)):(expandir (tail recursion) (drop 2 ls) (i+1) n)

instance Seq ListSeq where
   emptyS = []
   singletonS a = [a]
   lengthS ls = (length ls)
   nthS ls i = ls !! i
   tabulateS = map f [0..(n-1)]
   mapS f ls = map f ls
   filterS f ls = filter f ls
   appendS ls lb = ls ++ lb
   takeS ls n = take n ls
   dropS ls n = drop n ls
   --- es trabajo N log N.  (por take y drop) : ¿Entonces que onda, esta bien?
   showtS ls = let size = lengthS ls in
               case size of
                  0 -> EMPTY
                  1 -> ELT (nths ls 0)
                  _ -> let pot2 = logBase 2 (size-1)
                           (izq, der) = showtS (takeS ls pot2) ||| showtS (dropS ls pot2)
                       in NODE izq der
   showlS emptyS = NIL
   showlS (l:ls) = CONS l (showlS ls)
   joinS emptyS = emptyS
   joinS (l:ls) = appendS l (joinS ls)
   --reducetree f EMPTY no puede pasar
   reduceS f neutro emptyS = neutro
   reduceS f neutro ls = (f neutro (reducetree f (showtS ls)) ) -- como no es vacia, no hay emptys
  
  --- si no es asociativa que onda?
  --- convertiormos el problema a la mitad de chico,
  --- resolvememos ese problema. y con eso, resolvemos el grande.
   scanS f neutro emptyS  = ( [] , neutro )
   scanS f neutro [a] = ([neutro], f neutro a)
   scanS f neutro ls = let reduccion = emparejar f ls --- [x,y]
                           (recursion, total) = scans f neutro reduccion -- (<b, b*a1*a2>, b*a1*a2*a3*a4>)
                           expansion =  expandir recursion ls 0 (lengthS ls)
                       in (expansion, total)


 --  (a,b,c,d) -> (ab,cd) -> (abcd)
 --   ([neutro, neutroa, neutroab, neutro abc], neutroabcd)             ([neutro, neutroab] , neutroabcd)    ( [neutro] , neutroabcd)

 --  (a,b,c,d,e) -> (ab, cd, e) -> (abcd, e) -> (abcde)
 --                     ([neutro, neutroab, neutroabcd], neutroabcde) -> ([neutro,neutroabcd], neutroabcde)   ->  ([neutro], neutroabcde)


   fromList xs = xs 






