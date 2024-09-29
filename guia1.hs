-- Currificación y tipos

-- Ejercicio 1

max2 (x, y) | x >= y = x
            | otherwise = y

max2' x y = if x > y then x else y

normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorial' x y = sqrt (x^2 + y^2)

subtract = flip (-)

-- predecesor = subtract 1

evaluarEnCero = \f -> f 0

-- Ejercicio 2

curry' f x y = f (x,y)

uncurry' f (x,y) = f x y

-- Ejercicio 3

-- Redefinir usando foldr las funciones sum, elem, (++), filter y map

sum' [] = 0
sum' (x:xs) = foldr (+) x xs

elem' _ [] = False
elem' t xs = foldr (\x acc -> (x == t) || acc) False xs

concat' xs ys = foldr (\x acc -> x : acc) ys xs

concat'' xs ys = foldr (:) xs ys

filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

map' p xs = foldr (\x acc -> p x : acc) [] xs

-- Definir la función mejorSegun

-- mejorSegun' :: (a -> a -> Bool) -> [a] -> a
mejorSegun' p xs = foldr1 (\x acc -> if (p x acc) then x else acc)

-- Definir sumasParciales

-- sumasParciales :: [a] -> [a]
sumasParciales xs = reverse (snd (foldl (\(suma, acc) x -> (x + suma, (x + suma) : acc) ) (0,[]) xs))

-- Definir sumaAlt

sumaAlt xs = snd (foldr(\n (suma,resta) -> (n + resta, suma)) (0,0) (reverse xs)) - fst (foldr(\n (suma,resta) -> (n + resta, suma)) (0,0) (reverse xs))

sumaAlt' xs = fst (foldr(\n (res,i) -> (if even i then res + n else res - n, i + 1)) (0,0) (reverse xs))

-- Definir sumaAlt2 (lo mismo pero al revés)

sumaAlt2 xs = fst (foldr (\n (res,i) -> (if even i then res + n else res - n, i + 1)) (0,0) xs)

-- Ejerecicio 4

-- Definir permutaciones

insertarX x [] = [[x]]
insertarX x xs = [take i xs ++ [x] ++ drop i xs | i <- [0.. length xs]]

permutaciones [] = [[]]
permutaciones (x:xs) = concatMap (insertarX x) (permutaciones xs)

-- Definir partes

subconjN 0 _ = [[]]
subconjN n [] = []
subconjN n (x:xs) = (map (x:) (subconjN (n-1) xs)) ++ subconjN n xs

partes [] = [[]]
partes xs = foldr (++) [] ([subconjN n xs | n <- [0..length xs]])

-- Definir prefijos

prefijos [] = [[]]
prefijos xs = [take i xs | i <- [0.. length xs]]

-- Definir sublistas

sublistasN _ [] = []
sublistasN 0 xs = [[]]
sublistasN n xs = [take n (drop i xs) | i <- [0..length xs - n]]

sublistas [] = [[]]
sublistas xs = foldl (\x -> (++) x) [] [sublistasN n xs | n <- [0..length xs]]

-- Ejercicio 5

elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

    -- es recursión estructural puesto que la recursión la hace sobre una estructura de datos (tail xs)

elementosEnPosicionesPares' [] = []
elementosEnPosicionesPares' xs = fst (foldr (\x (acc, i) -> (if even i then x : acc else acc, i + 1)) ([], 0) xs)

entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

{- no es estructural debido a que aplica la recursión usando otra función que toma la segunda lista, 
    en vez de aplicarla directamente sobre la cola de la primer lista. Usa cosas de recursión estructural 
    pero al combinar dos listas deja de serlo -}

-- Ejercicio 6

{-  recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
    recr _ z [] = z
    recr f z (x : xs) = f x xs (recr f z xs) -}

sacarUna _ [] = []
sacarUna n (x:xs) = if (x == n) then xs else x : sacarUna n xs

sacarUna' _ [] = []
sacarUna' n xs = foldr (\x acc -> if x /= n then x : acc else acc) [] xs

{-  foldr no es adecuado ya que recorre la lista de derecha a izquierda 
    y lo que queremos nosotros es sacar la primer aparición de n en la 
    lista, ese problema lo podríamos resolver usando reverse. Sin embargo, 
    al recorrer toda la lista sacaríamos todas sus apariciones sin poder 
    detenernos una vez que encontramos al elemento la primera vez. -}

insertarOrdenado n [] = [n]
insertarOrdenado n (x:xs) = if (n > x) then x : insertarOrdenado n xs else n : x : xs

-- Ejercicio 7

-- genLista inicio func cant 

genLista _ _ 0 = []
genLista i f c = i : genLista (f i) f (c - 1) -- repasar

-- desdeHasta

desdeHasta (x,y) | x < y = genLista x (+1) y
                 | otherwise = []

-- Ejercicio 8

mapPares _ [] = []
mapPares f ys = map (\p -> (f (fst p) (snd p))) ys

mapPares' _ [] = []
mapPares' f ys = map (uncurry f) ys -- repasar

armarPares (x:xs) (y:ys) =  (x,y) : armarPares xs ys
armarPares _ _ = []

mapDoble f xs ys = mapPares' f (armarPares xs ys) -- B)

-- Ejercicio 9

matriz1 = [[1,2,3],[4,5,6],[7,8,9]]

sumaMat xs ys = zipWith (zipWith (+)) xs ys -- repasar

-- repasar, resolver usando foldl

trasponer' [] = []
trasponer' (x:xs) = head(x) : trasponer' xs

sacarPrimero xs = [map (drop i) xs | i <- [0.. (length xs - 1)]]

trasponer xs = map (trasponer') (sacarPrimero xs)

-- Ejercicio 10 

foldNat f base 0 = base
foldNat f base n = n f (foldNat f base (n - 1))

potencia' n 0 = 1
potencia' n e = n * potencia' n (e-1)

potencia n e = foldNat (\_ acc -> n * acc) 1 e -- repasar, ta mal :(

-- Ejercicio 11

unoxuno xs = map (\x -> [x]) xs

sufijos [] = []
-- sufijos xs = [drop i xs | i <- [0.. length xs]]
sufijos xs = foldr (\x acc -> [[x] ++ head acc]) [""] xs

-- sufijos' [] = [[]]
-- sufijos' xs = xs : sufijos (tail xs)

-- roseEjemplo = Rose 1 [Rose 2 [], Rose 3 [Rose 6 []], Rose 4 [Rose 5 []]]
