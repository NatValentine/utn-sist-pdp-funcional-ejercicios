import Text.Show.Functions

-- Determinar el prototipo/tipo de cada una de las funciones: 

a :: (a -> Bool ) -> [a] -> a 
a n = (head.filter n)

b :: Ord b => (a -> b) -> [(a,  a)] -> Bool 
b x y = (x.fst.head) y > (x.snd.head) y

c :: Eq t => (c -> t) -> c -> c -> Bool
c f a b = f a == f b

d :: (b -> Bool) -> (a -> b) -> Int -> [a] -> Bool
d x y z = (> z).length.filter x.map y

e :: Eq y => y -> [(x, y, z)] -> (x, y, z)
e nom = head.filter ((nom==).g)

f :: (x, y, z) -> y   
f (_, c, _) = c

g :: (Ord b, Num b, Num t) => t -> (t -> b) -> [t] -> t
g x _ [] = x
g x y (z:zs) | y z > 0 = z + f x y zs
             | otherwise = f x y zs

-- h :: 
h a b (c:cs) | a > c = f a b cs
             | otherwise = b c

-- i :: 
i a b c d e | (a.d e) (1, True) = 0
            | otherwise	= length (b:c) + e

j :: Eq b => (c -> b) -> b -> [c] -> c
j f x l = (head.filter ((x==).f)) l

qfsort :: Ord b => (a -> b) -> [a] -> [a]
qfsort f [] = []
qfsort f (x:xs) = (qfsort f (filter ((> f x).f) xs)) ++ [x] ++ (qfsort f (filter ((< f x).f) xs))

fLoca :: Num a => (b -> Bool) -> (a -> b) -> a -> [a]
fLoca g f n | (g . f ) n = n : fLoca g f (n + 1)
            | otherwise = fLoca g f (n + 1)

func :: Ord b => (a -> b) -> (b -> a -> Bool) -> b -> [a] -> Bool
func k p r = all (p r).filter ((> r).k)

otraFunc :: Eq a => a -> (b -> c -> a) -> b -> [(d, c)] -> Bool
otraFunc h r x = any (h==).map(\(_,z) -> r x z)
