import Data.List
--data NdTree p = Node (NdTree p) -- sub ́arbol izquierdo
--p -- punto
--(NdTree p) -- sub ́arbol derecho
--Int -- eje
-- |Empty
--deriving (Eq, Ord, Show)
data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty deriving Show

-- Ahora definimos una clase Punto. ¿Para que? Para no trabajar mas con el constructor del tipo de dato, sino
-- que con funciones que nos da la interfaz

class Punto p where
        dimension :: p -> Int -- devuelve el n ́umero de coordenadas de un punto
        coord :: Int -> p -> Double -- devuelve la coordenada k- ́esima de un punto (comenzando de 0)
        dist :: p -> p -> Double -- calcula la distancia entre dos puntos


distaux _ _ 0 = 0
distaux p1 p2 n = let r = ( (coord n p2) - (coord n p1) ) in ( (r*r) + ( distaux p1 p2 (n-1) )  )

distn p1 p2 =  (sqrt (distaux p1 p2 (dimension p1)) ) -- suponemos que ambos tienen la misma dimension.


newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

instance Punto Punto2d where
        dimension p = 2
        coord 0 (P2d (x,y)) = x
        coord 1 (P2d (x,y)) = y
        dist p1 p2 = distn p1 p2

instance Punto Punto3d where
        dimension p = 3
        coord 0 (P3d (x ,y, z) ) = x
        coord 1 (P3d (x ,y, z) ) = y
        coord 2 (P3d (x ,y, z) ) = z
        dist p1 p2 = distn p1 p2


--- queremos que ordene segun una coordenada distinta por nivel, para esto, usamos nivel%dimension

--- separar con take y drop

fromlistaux:: Punto p => [p] -> Int -> NdTree p
fromlistaux [] _ = Empty
fromlistaux (x:xs) level =  
   let coordcomp = (mod level ( dimension x) )
       array = sortBy (\x y ->  compare (coord coordcomp x) (coord coordcomp y)  ) (x:xs)
       median = head( drop (div (length array) 2) array )
       l = take (div (length array) 2) array
       r = drop ( (div (length array) 2)+1) array
       subl = fromlistaux l (level+1)
       subr = fromlistaux r (level+1) 
  in ( Node subl median subr coordcomp)


fromList :: Punto p => [p] -> NdTree p
fromList a = fromlistaux a 0



insertaraux p Empty level = (Node Empty p Empty level)
insertaraux p 

insertar :: Punto p -> p -> NdTree p -> NdTree p




   
