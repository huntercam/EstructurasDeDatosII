import Control.Exception
import Data.List
import Data.Maybe
---import Control.Deepseq

data Tree a = Empty | N (Tree a) (Tree a) a deriving Show

pot2_n n num = if (num*2) > n then num else pot2_n n (num*2)



balanceadoaux:: a -> Int -> Int -> Int -> (Tree a)
balanceadoaux dato l r hasta = if (r-l) > 1 then 
         (N (balanceadoaux dato ( div (l+r) 2) r hasta) (balanceadoaux dato l ( div (l+r) 2) hasta) dato) else 
		 if l <= hasta then 
		 (N Empty Empty dato) 
		 else Empty 


altura Empty = 0
altura (N l r x) = 1 + (max (altura l)  (altura r))

balanceado:: a -> Int -> (Tree a)
balanceado dato n = balanceadoaux dato (pot2_n n 1) ((pot2_n n 1)*2) n




-- usamos el tree

type BST a = Tree a


insertBST:: (Ord p) => p -> Tree p -> Tree p
insertBST p Empty = (N Empty Empty p)
insertBST p (N l r x)  = if p > x then (N l (insertBST p r) x) else (N (insertBST p l) r x)
 
joinBST:: (Ord p) => Tree p -> Tree p -> Tree p
joinBST Empty b = b
joinBST a Empty = a
joinBST a (N l r x) = joinBST (insertBST x a) (joinBST l r)

--joinBST a (N l r x) =  insertBST x (joinBST (joinBST l a) r)

  

split:: (Ord p) => Tree p -> p -> (Tree p , Tree p)
split Empty x = (Empty, Empty)
split (N l r a) x = 
     let ls = (split l x)
         rs = (split r x) 	 
     in
	 if a > x then
	 ( joinBST (fst ls) (fst rs) , insertBST a (joinBST (snd ls) (snd rs)) )
	 else
	 ( insertBST a (joinBST (fst ls) (fst rs)) , joinBST (snd ls) (snd rs) )



member :: (Ord a, Eq a) => a -> BST a -> Bool 
member a Empty = False
member a (N l r b)  | a == b    = True
                    | a < b     = (member a l)
                    | otherwise = (member a r)
					  
--- quiero menos comparaciones.
member2 :: (Ord a, Eq a) => a -> BST a -> [a] -> Bool 
member2 a Empty [] = False
member2 a Empty (p:ps) = ( a == p )
member2 a (N l r b) p | a <= b   = (member2 a l (b:p) )
                      |otherwise = (member2 a r p) 



ta = insertBST 10 $ insertBST 5 $ insertBST 1 $ insertBST 2 Empty

siBST = N (N (N Empty Empty 3)  (N Empty Empty 6) 5) (N Empty Empty 13) 10




data Color = R | B deriving (Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show, Eq)

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r) | a == b = True
                      | a < b = memberRBT a l
                      | a > b = memberRBT a r


--- INV1 Ningun nodo rojo tiene hijos rojos.
--  INV2 Todos los caminos de la raiz a una hoja tienen el mismo numero de nodos negros (altura negra).


makeBlack E = E
makeBlack (T _ l x r) = T B l x r

--chequeda que el invariante 1 se cumple en los subarboles izquierdos
lbalance (T c l y r) = balance c l y r


insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
             where ins x E = T R E x E
                   ins x a@(T c l y r) | x < y = T c (lbalance (ins x l)) y r
                              | x > y = T c l y (lbalance (ins x r))
                              | otherwise = a
                              

color :: RBT a -> Color
color (T x _ _ _) = x

data Arbol123 a = Vacio123
                | Nodo2 a (Arbol123 a) (Arbol123 a) 
                | Nodo3 a a (Arbol123 a) (Arbol123 a) (Arbol123 a) 
                | Nodo4 a a a (Arbol123 a) (Arbol123 a) (Arbol123 a) (Arbol123 a)
                deriving Show

rbt123tree :: RBT a -> Arbol123 a
rbt123tree E                           = Vacio123
rbt123tree (T R _ _ _) | (div 1 0) > 0 = Vacio123 --- bromita.. jajajaja
rbt123tree (T _ E a E)                 = (Nodo2 a Vacio123 Vacio123)
rbt123tree (T _ E a (T R h1 b h2))     = (Nodo3 a b Vacio123 (rbt123tree h1) (rbt123tree h2))
rbt123tree (T _ E a r)                 = (Nodo2 a Vacio123 (rbt123tree r))
rbt123tree (T _ (T R h1 b h2) a E)     = (Nodo3 a b Vacio123 (rbt123tree h1) (rbt123tree h2))
rbt123tree (T _ l a E)                 = (Nodo2 a Vacio123 (rbt123tree l))
rbt123tree (T _ l@(T R h1 b h2) a r@(T B h3 c h4)) = (Nodo3 a b (rbt123tree h1) (rbt123tree h2) (rbt123tree l))
rbt123tree (T _ l@(T B h1 b h2) a r@(T R h3 c h4)) = (Nodo3 a b (rbt123tree h1) (rbt123tree h2) (rbt123tree l))
rbt123tree (T _ l@(T R h1 b h2) a r@(T R h3 c h4)) = (Nodo4 a b c (rbt123tree h1) (rbt123tree h2) (rbt123tree h3) (rbt123tree h4))
rbt123tree (T _ l a r)                             = (Nodo2 a (rbt123tree l) (rbt123tree r))




type Rank = Int
data Heap a = Eheap | Nheap Rank a (Heap a) (Heap a) deriving Show



insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x h = merge (Nheap 1 x Eheap Eheap) h

findMin :: Ord a => Heap a -> a
findMin (Nheap r x a b) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Nheap r x a b) = merge a b


rank Eheap = 0
rank (Nheap r x u v) = r

makeH x a b = 
    if (rank a) >= (rank b) then Nheap (rank b + 1) x a b
                            else Nheap (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Eheap = h1
merge Eheap h2 = h2
merge h1@(Nheap r1 x a1 b1) h2@(Nheap r2 y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
              else makeH y a2 (merge h1 b2)


listToHeap [] = Eheap
listToHeap (x:xs) = insertHeap x (listToHeap xs)

pepe = [1,2,3,4,5,4,3,2,3,4,5,5,4,3,2,1,5,5,4,3,2,1,5,6,7,3,4,5,9,3,2,2,2] -- 17 elementos

tata = insertHeap 1 Eheap

alturaHeap Eheap = 0
alturaHeap (Nheap r x a b) = 1 + ( max (alturaHeap a) (alturaHeap b))

aux = listToHeap pepe

heapToList Eheap = []
heapToList (Nheap r x a b) = x:( (heapToList a)++(heapToList b))

data PHeap a = Vacio | Raiz a [PHeap a] deriving Show

valorPHeap (Raiz a _) = a

isPHeap :: Ord a => PHeap a -> Bool
isPHeap Vacio = True
isPHeap (Raiz a hs) = all (\x -> (isPHeap x) && (valorPHeap x) >= a) hs

monticuloDePij_s = (Raiz 0 [(Raiz 2 [(Raiz 4 []), (Raiz 10 [])]), (Raiz 11 [])])
noSoyUnMontonDePij_s = (Raiz 1000 [(Raiz 1002 [(Raiz 1004 []), (Raiz 1000 [])]), (Raiz 1011 [])])


mergeHeap:: (Ord a ) => PHeap a -> PHeap a -> PHeap a
mergeHeap u Vacio = u
mergeHeap Vacio v = v
mergeHeap r1@(Raiz a h1) r2@(Raiz b h2) = if (a < b) then (Raiz a (r2:h1) ) else (Raiz b (r1:h2) )

insertPHeap :: (Ord a) => PHeap a -> a -> PHeap a
insertPHeap Vacio a = Raiz a []
insertPHeap (Raiz a hs) b = if b < a then (Raiz b ((Raiz a []):hs)) else (Raiz a ((Raiz b []):hs))

concatHeaps :: (Ord a) => [PHeap a] -> PHeap a
concatHeaps [] = Vacio
concatHeaps [x] = x
concatHeaps (x:xs) = mergeHeap x (concatHeaps xs)

delMin :: (Ord a) => PHeap a -> Maybe (a, PHeap a)
delMin Vacio = Nothing
delMin (Raiz a hs) = Just (a, concatHeaps hs)