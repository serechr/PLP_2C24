-- Currificación y tipos

-- Ejercicio 1

max2 (x, y) | x >= y = x
            | otherwise = y

max2' x y = if x > y then x else y

normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorial' x y = sqrt (x^2 + y^2)

subtract = flip (-)

predecesor = subtract 1

evaluarEnCero = \f -> f 0
