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

reducetree:: (a -> a -> a) -> TreeView a (Arr a) -> a
reducetree f (ELT x) = x
reducetree f (NODE ls rs) = let (ca,cb) =  (reducetree f (showtS ls)) ||| (reducetree f (showtS rs))
                        in  (f ca cb)

expandir:: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
expandir f comprimido ls i n | i >= n       = []
                             | i `mod` 2 == 0 = (head comprimido):(expandir f comprimido ls (i+1) n)
                             | i `mod` 2 == 1 = (f (head comprimido) (head ls)):(expandir f (tail comprimido) (drop 2 ls) (i+1) n)


append a b = flatten (Arr.fromList [a, b])
appendPar (a, b) = flatten (Arr.fromList [a, b])

emparejar :: (a -> a -> a) -> Arr a -> [a]
emparejar f ar = let len = lengthS ar in
    case len of 
        0 -> []
        1 -> [nthS ar 0]
        _ -> (f (nthS ar 0) (nthS ar 1)):(emparejar f (dropS ar 2))

instance Seq Arr where
    emptyS = empty 
    singletonS a = Arr.fromList [a]
    lengthS ar = Arr.length ar
    nthS ar i = ( (!) ar  i)
    tabulateS f n = tabulate f n
    fromList xs = Arr.fromList xs
    appendS ar br = flatten (Arr.fromList [ar, br])
    takeS ar n = subArray 0 n ar

    dropS ar n = subArray n (lengthS ar - n) ar
    
    showtS ar = let len = lengthS ar in
        case len of
            0 -> EMPTY
            1 -> ELT (nthS ar 0) 
            _ -> NODE (takeS ar (div len 2) ) ( dropS ar (div len 2) )
    
    showlS ar = let len = lengthS ar in
        case len of
          0 -> NIL
          _ -> CONS (nthS ar 0) (dropS ar 1)

    

    joinS arr = flatten arr

    reduceS f neutro ar = let len = lengthS ar in 
        case len of
            0 -> neutro 
            _ -> f neutro (reducetree f (showtS ar))
    
    scanS f neutro ar = let len = lengthS ar in
      case len of
          0  -> (emptyS, neutro)
          1  -> (singletonS neutro, f neutro (nthS ar 0))
          _ -> 
            let reduccion = (emparejar f ar)
                (recursion, total) = scanS f neutro reduccion
                expansion =  expandir f recursion ar 0 (lengthS ar)
            in (expansion, total)
       
    
    mapS f ar = let l = lengthS ar in
        appendPar (
            mapS f (takeS ar (div l 2) ) |||
            mapS f (dropS ar (div l 2) )
        )

    filterS f ar = let len = lengthS ar in
                case len of 
                 1 ->  if (f (nthS ar 0) ) then  singletonS (nthS ar 0) else emptyS
                 _ -> appendPar (
                            filterS f (takeS ar (div len 2) ) |||
                            filterS f (dropS ar (div len 2) )
                        )