{-
 Este módulo requiere la librería Vector. 
 
 Esta puede instalarse utilizando Cabal, ejecutando el siguiente código  
 en un intérprete de comandos: 
 
 $ cabal update
 $ cabal install vector
 
-}

module ArrSeq where
  
import Arr
import Par
import Seq

reducetree:: (a -> a -> a) -> TreeView a [a] -> a
reducetree f (ELT x) = x
reducetree f (NODE ls rs) = let (ca,cb) =  (reducetree f (showtS ls)) ||| (reducetree f (showtS rs))
                        in  (f ca cb)

append a b = flatten (fromList [a, b])
appendPar (a, b) = flatten (fromList [a, b])

emparejar :: (a -> a -> a) -> Arr a -> [a]
emparejar f ar = let len = lengthS ar in
    | len == 0  = []
    | len == 1  = [nthS ar 0]
    | otherwise = (f (nthS ar 0) (nthS ar 1)):(emparejar f (dropS ar 2))

instance Seq Arr where
    emptyS = empty
    singletonS a = fromList [a]
    lengthS ar = length ar
    nthS ar i = ! ar  i
    tabulateS f n = tabulate f n

    appendS ar br = flatten (fromList [a, b])
    takeS ar n = subArray 0 n ar

    dropS ar n = subArray n (lengthS ar - n) ar
    
    showtS ar = let len = lenghtS ar in
                | len == 0 = EMPTY
                | len == 1 = ELT (nthS ar 0) 
                | otherwise = NODE (takeS ar (div len 2) ) ( dropS ar (div len 2) )
    
    showlS ar = let len = lengthS ar in
        | len == 0  = NIL
        | otherwise = CONS (nthS ar 0) (dropS ar 1)

    fromList xs = fromList xs --- Maybe PROBLEMAS

    joinS arr = flatten arr

    reduceS f neutro ar = let len = lengthS ar in 
        | len == 0  = neutro 
        | otherwise = f neutro (reducetree f (showtS ar))
    
    scanS f neutro ar = let len = lengthS ar in
        | len == 0  = ([], neutro)
        | len == 1  = ([neutro], f neutro (nthS ar 0))
        | otherwise = 
            let reduccion = (emparejar f ls)
                (recursion, total) = scanS f neutro reduccion
                expansion =  expandir f recursion ls 0 (lengthS ls)
            in (expansion, total)
       
    
    mapS f ar = let l = length ar in
        appendPar (
            mapS f (take (div l 2) ar) |||
            mapS f (drop (div l 2) ar)
        )

    filterS f ar = let l = lengthS ar in
                  | l == 1 = if (f (nths 0 ar) ) then [ (nths 0 ar) ] else []
                  | otherwise = appendPar (
                            filterS f (take (div l 2) ar) |||
                            filterS f (drop (div l 2) ar)
                        )