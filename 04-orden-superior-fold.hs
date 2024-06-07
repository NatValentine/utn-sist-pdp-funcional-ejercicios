import Text.Show.Functions

-- Ejercicio 1:
-- Dada una lista de tuplas, sacar la cantidad de elementos utilizando foldl y foldr.
-- Ej: cantidadDeElementos [(8,6),(5,5),(5,6),(7,8)] = 4
cantidadDeElementos :: [(Integer, Integer)] -> Integer
cantidadDeElementos lista = foldl (\sem _ -> sem + 1)  0 lista

cantidadDeElementos' :: [(Integer, Integer)] -> Integer
cantidadDeElementos' lista = foldr (\_ sem -> sem + 1) 0  lista


-- Ejercicio 2: 
-- Dada una lista de pares (empleado, gasto), conocer el empleado más gastador usando foldl y foldr.
-- Ej: masGastador [("ana",8000),("pepe",4000),("juan",30000),("maria",12000)] = ("juan",30000)
masGastador :: [(String, Integer)] -> (String, Integer)
masGastador   (cab:cola)   = foldl mayorGasto  cab cola

mayorGasto :: (String, Integer) -> (String, Integer) -> (String, Integer)
mayorGasto empleado otroEmpleado | snd empleado > snd otroEmpleado = empleado
                                 | otherwise = otroEmpleado

masGastador' :: [(String, Integer)] -> (String, Integer)
masGastador' (x:xs) = foldr mayorGasto x xs


-- Ejercicio 3: 
-- Dada una lista de (empleado, gasto), conocer el gasto total usando foldl y foldr.
-- Ej: monto  [("ana",8000),("pepe",4000),("juan",30000),("maria",12000)] = 54000
monto :: [(String, Integer)] -> Integer
monto empleados = foldl (\sem (_, gasto) -> sem + gasto) 0 empleados

monto' :: [(String, Integer)] -> Integer
monto' empleados = foldr (\(_, gasto) sem -> gasto + sem) 0 empleados


-- Ejercicio 4:
-- Completar con lo que corresponda para:
    -- > foldl …. 2 [(3+), (*2), (5+)] = 15
    -- foldl (\sem fun -> fun sem ) 2 [(3+), (*2), (5+)]
    
    -- > foldr …. 2 [(3+), (*2), (5+)] = 17
    -- foldr (\fun sem -> fun sem)  2 [(3+), (*2), (5+)]


-- Ejercicio 5:
-- Dada una lista de proyectos

type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos :: [Proyecto]
proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 1000 ["cocinero"] ]

-- Determine una función que permita conocer el máximo proyecto según. Revolverlo usando foldl y foldr.
maximoProySegun :: ( Proyecto -> Int) -> [Proyecto] -> Proyecto 
maximoProySegun f (proyecto : proyectos) = foldl (maximoSegun f)  proyecto  proyectos

maximoSegun :: ( Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto > f otroProyecto = unProyecto
                                      | otherwise = otroProyecto

maximoProySegun' :: ( Proyecto -> Int) -> [Proyecto] -> Proyecto 
maximoProySegun' f (proyecto : proyectos) = foldr (maximoSegun f)  proyecto  proyectos

maximoProySegun'' :: ( Proyecto -> Int) -> [Proyecto] -> Proyecto 
maximoProySegun'' f proyectos = foldl1 (maximoSegun f) proyectos

    -- La inversión inicial
        -- > maximoProySegun inversionInicial proyectos
        -- Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}

    -- El nro de profesionales
        -- > maximoProySegun (length.profesionales) proyectos
        -- Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]}

    -- La cantidad de palabras del proyecto
        -- > maximoProySegun (length.words.nombre) proyectos
        -- Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
