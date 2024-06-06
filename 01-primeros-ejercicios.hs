-- Ejemplos
siguiente :: Integer -> Integer
siguiente nro = nro + 1

doble :: Integer -> Integer
doble nro = nro * 2

-- Ejercicio 1:
-- Definir la función calcular', que recibe una tupla de 2 elementos, y devuelve una nueva tupla según las siguientes reglas:
-- - Si el primer elemento es par, lo duplica; si no, lo deja como está
-- - Si el segundo elemento es impar le suma 1; si no, lo deja como está
calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (unNro, otroNro) = (calcularPrimerNro unNro, calcularSegundoNro otroNro)

calcularPrimerNro :: Integer -> Integer
calcularPrimerNro nro | even nro = doble nro
                      | otherwise = nro

calcularSegundoNro :: Integer -> Integer
calcularSegundoNro nro | even nro = nro
                       | otherwise = nro + 1

-- Ejercicio 2:
-- Definir las funciones booleanas estándar sin usar las funciones predefinidas.
and' :: Bool -> Bool -> Bool
and' unValor otroValor | unValor = otroValor
                       | otherwise = False

or' :: Bool -> Bool -> Bool
or' unValor otroValor | unValor = True
                      | otherwise = otroValor

-- Ejercicio 3:
-- Definir la función notaMaxima que dado un alumno devuelva la máxima nota del alumno.
-- Resolver sin usar guardas y teniendo en cuenta los siguientes tipos:
type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota1, nota2, nota3) = nota1 `max` (nota2 `max` nota3)

-- Ejercicio 4:
-- Definir la función cuadruple reutilizando la función doble. 
cuadruple :: Integer -> Integer
cuadruple = doble.doble

-- Ejercicio 5:
-- Definir la función esMayorA, que verifique si el doble del siguiente de la suma entre 2 y un número es mayor a 10. 
esMayorA :: Integer -> Bool
esMayorA = (>10).doble.siguiente.(+2)

-- Ejercicio 6: Definir las siguientes funciones utilizando expresiones lambda:
triple :: Integer -> Integer
triple = \x -> x * 3

siguiente' :: Integer -> Integer
siguiente' = \x -> x + 1

suma :: Integer -> Integer -> Integer
suma = \x y -> x + y

sumarDos :: Integer -> Integer
sumarDos = \x -> x + 2

-- Ejercicio 7:
-- Dadas las siguientes funciones:
-- 7.1. apply f x = f x
    -- ¿A qué se reduce la siguiente expresión?
    -- apply fst (const 5 7, 4)
    -- rta: 5

-- 7.2 twice f x = (f.f) x
    -- ¿A qué se reduce la siguiente expresión?
    -- twice (`div` 2) 12
    -- rta: 3

-- Ejercicio 8:
-- Representamos las notas que sacó un alumno en dos parciales mediante un par (nota1, nota2).
-- A partir de esto:
-- 8.1. Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No usar guardas. 
-- 8.2. Definir la función aprobado, recibe un par e indica si una persona que se sacó esas notas aprueba. 
-- 8.3. Definir la función promocionado, que indicá si promocionó, para eso:
    -- Las notas tienen que sumar al menos 16
    -- Tiene que haberse sacado al menos un 8 en cada parcial 
-- 8.4. Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición.
    -- La consulta tiene que tener esta forma: (... algo ...) (5, 8)


-- Ejercicio 9:
-- Queremos calcular el sueldo de los empleados de nuestra empresa. Tenemos dos tipos de empleado:
    -- Los comunes: nos interesa saber el sueldo básico y el nombre. 
    -- Los jerárquicos: nos interesa conocer el sueldo básico, la cantidad de gente a cargo y el nombre. 
-- El sueldo que cobran los comunes se determina por el sueldo básico. 
-- El sueldo de los jerárquicos se calcula como sueldo básico + plus cantidad de gente a cargo (5000 por cada)
-- Ejemplo:
-- > sueldo (Jefe 50000 3 "Sonia")
-- 65000


-- Ejercicio 10:
-- Se conocen estas bebidas:
data Bebida = Cafe { nombreBebida :: String } | Gaseosa { sabor :: String, azucar :: Integer }
-- Dado un producto determinar si es energizante. 
    -- Si es café, es energizante si es un capuccino.
    -- Si es gaseosa, es energizante si es sabor pomelo y tiene más de 10gr de azúcar. 
esEnergizante :: Bebida -> Bool
esEnergizante (Cafe nom) = nom == "capuccino" 
esEnergizante (Gaseosa sabor azucar) = sabor == "pomelo" && azucar > 10
