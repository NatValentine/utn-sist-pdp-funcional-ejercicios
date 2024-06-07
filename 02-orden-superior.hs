import Text.Show.Functions

-- Ejercicio 1:
-- Resolver la función find’ que encuentra el primer elemento que cumple una condición. No usar recursividad. 
-- Si ningún elemento cumple la condición dejar que falle.
-- Por ejemplo:
-- find’ :: (a -> Bool) -> [a] -> a
-- > find' even [41..339]
-- > 42
find' :: (a -> Bool) -> [a] -> a
find' f = head.filter f

-- 1.1. Aprovechar la función find’ para aplicarla a este dominio:
data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edad :: Int} deriving Show 

politicos :: [Politico]
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

    -- Queremos encontrar:
        -- un político joven (menos de 50 años)
            -- > find' ((<50).edad) politicos
        -- alguno que haya presentado más de 3 proyectos
            -- > find' ((>3).length.proyectosPresentados) politicos
        -- alguno que haya presentado algún proyecto que tenga más de 3 palabras
            -- > find' (any((>3).length.words) .proyectosPresentados) politicos


-- Ejercicio 2:
-- Definir la función promediosAlumnos/1, que dada una lista de alumnos devuelve una lista de tuplas que tenga el alumno y el promedio (Consideramos la división entera para el promedio y usamos la funcion div).
type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}
-- Ejemplo:
-- > promediosAlumnos[(Alumno "juan" [8,6]), (Alumno "maria" [7,9,4]), (Alumno "ana" [6,2,4])]
-- > [("juan",7),("maria",6),("ana",4)]

promedioAlumnos :: [Persona] -> [(Nombre, Int)]
promedioAlumnos = map (\(Alumno nombre notas) -> (nombre, promedio notas))

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)


-- Ejercicio 3:
-- Definir la función promediosSinAplazos/1, que dada una lista de listas, devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 6 que no se cuentan.
-- Ejemplo:
-- > promediosSinAplazos [[8,6],[6,6,4]]
-- > [7,6]

promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos = map (promedio.filter(>=6))

 
-- Ejercicio 4: 
-- Definir la función aprobó/1, que dado un alumno devuelve True si el alumno aprobó. 
-- Aclaración: se dice que un alumno aprobó si todas sus notas son 6 o más.
-- Ejemplo:
-- > aprobo (Alumno "manuel" [8,6,2,4])
-- > False

aprobo :: Persona -> Bool
aprobo = all(>=6).notas

 
-- Ejercicio 5:
-- Definir la función aprobaron/1, que dada una lista de alumnos, devuelve los nombres de los alumnos que aprobaron.
-- Ejemplo:
-- > aprobaron [Alumno "manuel" [8,6,2,4] , Alumno "elena" [7,9,8,7], Alumno "ana" [6,2,4,2], Alumno "pedro" [9,6,7,10]]
-- > ["elena", "pedro"]

aprobaron :: [Persona] -> [String]
aprobaron = map nombre.filter aprobo

 
-- Ejercicio 6: 
-- Definir la función productos que dado una lista de nombres  de productos y una lista de precios, devuelve una lista de tuplas.
-- Definirla usando zip y usando zipWith
-- Ejemplo:
-- > productos ["melon", "zapallo", "palta"] [ 15, 10, 12, 7]
-- > [("melon", 15), ("zapallo", 10), ("palta", 12)]

productos :: [String] -> [Int] -> [(String, Int)]
productos nombres precios = zip nombres precios

-- o bien:
productos' :: [String] -> [Int] -> [(String, Int)]
productos' nombres precios = zipWith (\nom pre -> (nom, pre))  nombres precios