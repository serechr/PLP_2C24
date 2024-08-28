-- Ejercicio 2

valorAbsoluto1 :: Float -> Float
valorAbsoluto1 n = if n < 0 then -n else n

valorAbsoluto2 :: Float -> Float
valorAbsoluto2 n | n < 0 = -n
                 | otherwise = n

bisiesto :: Int -> Bool
bisiesto n = (mod n 4 == 0 && mod n 100 /= 0) || mod n 400 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * factorial(n - 1)

divisores :: Int -> [Int]
divisores n = filter (\f -> mod n f == 0) [1..n]

esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2

primos :: [Int] -> [Int]
primos xs = filter (\x -> esPrimo(x)) xs

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (primos(divisores n))

-- Ejercicio 3


inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1/n) 

aEntero :: Either Int Bool -> Int
aEntero (Left _) = -1
aEntero (Right True) = 1
aEntero (Right False) = 0

-- Ejercicio 4

limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x:xs) ys = limpiar xs (filter (/= x) ys)

longitud :: [t] -> Float
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

promedio :: [Float] -> Float
promedio xs = sum(xs) / longitud(xs)

difPromedio :: [Float] -> [Float] 
difPromedio xs = map (\x -> x - promedio xs) xs

todosIguales1 :: [Int] -> Bool
todosIguales1 [] = True
todosIguales1 [x] = True
todosIguales1 (x:y:xs) = x == y && todosIguales1 xs

todosIguales2 :: [Int] -> Bool
todosIguales2 [] = True
todosIguales2 (x:xs) = all (== x) xs

todosIguales3 :: [Int] -> Bool
todosIguales3 [] = True
todosIguales3 (x:xs) = length (filter (\y -> y == x) (x:xs)) == length (x:xs)

-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB a = a == Nil

vacioAB2 :: AB a -> Bool
vacioAB2 Nil = True
vacioAB2 (Bin _ _ _) = False

negacionAB :: AB Bool → AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d) 

productoAB :: AB Int → Int
productoAB Nil = 1
productoAB (Bin i r d) = (productoAB i) * r * (productoAB d)