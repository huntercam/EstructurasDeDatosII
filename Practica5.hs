class List t where
	nil :: t
	cons :: t -> t a -> t a
	null :: t a -> Bool
	head :: t a -> a
	tail :: t a -> a

1-a)
null nil = True
null (cons a q) = False
head (cons a q) = a
tail (cons a q) = q

1-b)
null <> = True
null <x1,x2,..,xn> = False
head <x1,x2,..,xn> = x1
tail <x1,x2,..,xn> = <x2,..,xn>

4-
tad PQ (A : ordered set, P : ordered set) where
	import Bool
	vacia : PQ A P
	poner : A P -> PQ A P -> PQ A P
	sacar : PQ A P -> PQ A P
	primero : PQ A P -> A P
	esVacia : PQ A P -> Bool
	union : PQ A P -> PQ A P -> PQ A P

poner a pa (poner b pb pq) = poner b pb (poner a pa pq)

primero (poner a pa vacia) = a pa
primero (poner a pa (poner y pb pq)) = if pa > pb then primero (poner a pa pq)  else primero (poner y pb pq)

sacar (poner a pa vacia) = vacia
sacar (poner a pa (poner y pb pq)) = if pa > pb 
	then poner y pb (sacar (poner a pa pq))
	else poner a pa (sacar (poner b pb pq))

esVacia vacia = True
esVacia (poner a pa x) = False

union vacia y = y
union x vacia = x
union (poner a pa x) (poner b pb y) = (poner a pa (poner b pb (union x y))
---------------------
union   {(x1,px1),..,(xn,pxn)} {(xd1,pxd1),..,(xdn,pxdn)} = {(x1,px1),..,(xn,pxn),(xd1,pxd1),..,(xdn,pxdn)}
primero {(x1,px1),..,(xn,pxn)} = (xk,pxk) si pxk >= pxi para todo 1 <= i <= n
sacar   {(x1,px1),..,(xn,pxn)} = {(x1,px1),..,(xn,pxn)} - {primero {(x1,px1),..,(xn,pxn)}}
esVacia {} = True
esVacia {(x1,px1),..,(xn,pxn)} = False

5-
tad Bait (A : Ordered Set) where
	import Maybe, Int
	empty : Bait A
	join : Bait A -> Maybe A -> Bait A -> Bait A
	size : Bait A -> Int
	expose : Bait A -> Maybe (Bait A, A, Bait A)

join ( (join l x r )  x2 (join l3 x3 r3 ) )

size empty = 0
size (join l x r) = succ(0) + size l + size r

expose empty = Nothing
expose (join l x r) = Just (l, x, r) 







