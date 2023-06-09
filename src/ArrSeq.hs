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
reducetree f (NODE ls rs) = 
    let 
        (ca,cb) =  (reducetree f (showtS ls)) ||| (reducetree f (showtS rs))                   
    in
        (f ca cb)

expandir :: (a -> a -> a) -> Arr a -> Arr a -> Arr a
expandir f ar br =
    let
        fill i = if (i `mod` 2) == 0
            then   (nthS ar (i `div` 2))
            else f (nthS ar (i `div` 2)) (nthS br (i-1))
    in
        tabulate fill (lnegthS br)

append a b = flatten (Arr.fromList [a, b])
appendPar (a, b) = flatten (Arr.fromList [a, b])

emparejar :: (a -> a -> a) -> Arr a -> [a]
emparejar f ar = 
    let 
        len    = lengthS ar
        half   = div len 2
        unir i = if i < half 
            then f (nthS ar (2*i)) (nthS ar (2*i+1))
            else   (nthS ar (2*i)) --- en el caso de n impar
    in
        if (len `mod` 2) == 0 
        then
            tabulate unir  half    --- llama hasta (fpares half-1)
        else
            tabulate unir (half+1) --- llama hasta (fpares half)

instance Seq Arr where
    emptyS = empty
    singletonS a = Arr.fromList [a]
    lengthS ar = Arr.length ar
    nthS ar i = ( (!) ar  i)
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
    appendS ar br = flatten (Arr.fromList [ar, br])
    fromList xs = Arr.fromList xs
    joinS arr = flatten arr
    tabulateS f n = tabulate f n

    mapS f ar =
        let
            applyf i = f (nthS ar i)
        in
            tabulate applyf (lengthS ar)

    ---log + map / tabulate
    filterS f ar = joinS (mapS (\x-> if (f x) then [x] else [])  ar)

    reduceS f neutro ar = let len = lengthS ar in 
        case len of
            0 -> neutro 
            _ -> f neutro (reducetree f (showtS ar))

    scanS f neutro ar = let len = lengthS ar in
        case len of
            0 -> (emptyS, neutro)
            1 -> (singletonS neutro, f neutro (nthS ar 0))
            _ ->
                let reduccion = (emparejar f ar)
                    (recursion, total) = scanS f neutro reduccion
                    expansion =  expandir f recursion
                in (expansion, total)