import Data.List

--data NdTree p = Node (NdTree p) -- sub ́arbol izquierdo
--p -- punto
--(NdTree p) -- sub ́arbol derecho
--Int -- eje
-- |Empty
data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty deriving Show


distaux _ _ 0 = 0
distaux p1 p2 n = let r = ( (coord n p2) - (coord n p1) ) in ( (r*r) + ( distaux p1 p2 (n-1) )  )

class Punto p where
        dimension :: p -> Int 
        coord :: Int -> p -> Double
        dist :: p -> p -> Double
        dist p1 p2 =  (sqrt (distaux p1 p2 (dimension p1)) )
        menor :: p -> p -> Int -> Bool
        menor p1 p2 axis = (coord axis p1) < (coord axis p2)
        igual :: p -> p -> Bool



newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

instance Punto Punto2d where
        dimension p = 2
        coord 0 (P2d (x,y)) = x
        coord 1 (P2d (x,y)) = y
        dist p1 p2 = dist p1 p2
        menor p1 p2 axis = (coord axis p1) < (coord axis p2)
        igual (P2d a) (P2d b) = a == b

instance Punto Punto3d where
        dimension p = 3
        coord 0 (P3d (x ,y, z) ) = x
        coord 1 (P3d (x ,y, z) ) = y
        coord 2 (P3d (x ,y, z) ) = z
        dist p1 p2 = dist p1 p2
        menor p1 p2 axis = (coord axis p1) < (coord axis p2)
        igual (P3d a) (P3d b) = a == b


fromlistaux:: Punto p => [p] -> Int -> NdTree p
fromlistaux [] _ = Empty
fromlistaux (x:xs) level =  
   let axis = (mod level ( dimension x) )
       array = sortBy (\x y ->  compare (coord axis x) (coord axis y)  ) (x:xs)
       median = head( drop (div (length array) 2) array )
       l = take (div (length array) 2) array
       r = drop ( (div (length array) 2)+1) array
       subl = fromlistaux l (level+1)
       subr = fromlistaux r (level+1) 
  in ( Node subl median subr axis)


fromList :: Punto p => [p] -> NdTree p
fromList a = fromlistaux a 0


insertaraux p Empty prevaxis = (Node Empty p Empty (mod (prevaxis + 1) (dimension p)))
insertaraux p (Node l v r axis) prevaxis = if (coord axis p) > (coord axis v) then (Node l v (insertaraux p r axis) axis) else (Node (insertaraux p l axis) v r axis)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p tree = insertaraux p tree 0


tolist :: NdTree p -> [p]
tolist Empty = []
tolist (Node l a r axis) = (tolist l) ++ [a] ++ (tolist r)

compareMAX:: (Punto p) => Int -> p -> p -> p
compareMAX axis p1 p2 = if (coord axis p1) > (coord axis p2) then p1 else p2

maximo :: (Punto p) => Int -> NdTree p -> p
maximo axis (Node Empty a Empty _) = a
maximo axis (Node Empty a r _) = compareMAX axis (maximo axis r) a
maximo axis (Node l a Empty _) = compareMAX axis (maximo axis l) a
maximo axis (Node l a r _) = compareMAX axis (maximo axis r) (compareMAX axis (maximo axis l) a)

compareMIN:: (Punto p) => Int -> p -> p -> p
compareMIN axis p1 p2 = if (coord axis p1) < (coord axis p2) then p1 else p2

minimo :: (Punto p) =>  Int -> NdTree p -> p
minimo axis (Node Empty a Empty _) = a
minimo axis (Node Empty a r _) = compareMIN axis (minimo axis r) a
minimo axis (Node l a Empty _) = compareMIN axis (minimo axis l) a
minimo axis (Node l a r _) = compareMIN axis (minimo axis l) (compareMIN axis (minimo axis r) a)


eliminar :: (Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p a@(Node Empty vA Empty axis) | igual p vA = Empty
                                        | otherwise = a
eliminar p a@(Node l vA Empty axis) | igual p vA   = let maximoL = (maximo axis l) in (Node (eliminar maximoL l) maximoL Empty axis)
                                    | menor p vA axis = (Node (eliminar p l) vA Empty axis)
                                    | otherwise = a

eliminar p a@(Node l vA r axis)     | igual p vA   = let minimoR = (minimo axis r) in (Node l minimoR (eliminar minimoR r) axis)
                                    | menor p vA axis  = (Node (eliminar p l) vA r axis)
                                    | otherwise = (Node l vA (eliminar p r) axis)

type Rect = (Punto2d, Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion ( P2d (x, y) ) ( P2d (x0, y0) , P2d (x1, y1)  ) = x >= (min x0 x1) && x <= (max x0 x1) && y >= (min y0 y1) && y <= (max y1 y0)


ortogonalaux Empty _ = []
ortogonalaux (Node l a@(P2d (xa, ya)) r axis) reg@((P2d (xb, yb)),(P2d (xc, yc)))
        | inRegion a reg = a:((ortogonalaux l reg)++(ortogonalaux r reg))
        | axis == 0 && xa < xb = (ortogonalaux r reg)
        | axis == 0 && xa > xc = (ortogonalaux l reg)
        | axis == 0 = (ortogonalaux l reg)++(ortogonalaux r reg)
        | axis == 1 && ya < yb = (ortogonalaux r reg)
        | axis == 1 && ya > yc = (ortogonalaux l reg)
        | axis == 1 = (ortogonalaux l reg)++(ortogonalaux r reg)

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch a (P2d (xb, yb),P2d (xc, yc)) = ortogonalaux a (P2d (min xb xc, min yb yc),P2d (max xb xc, max yb yc))


lista = [ P2d (7.0,2.0) ,P2d (5.0,4.0),P2d (2.0,3.0), P2d (4.0,7.0), P2d (8.0,1.0), P2d (9.0,6.0)]
arbol = fromList lista
newlist = insertar (P2d (4.0,2.0)) arbol
rectangulo = ortogonalSearch arbol (P2d (2.0,2.0), P2d (8.0,4.0) )
w = eliminar (P2d (5.0,4.0)) (eliminar ( P2d (7.0,2.0) ) arbol)





   
