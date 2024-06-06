import Text.Show.Functions

-- Ejercicio 1:
-- Dada una lista de tuplas, sacar la cantidad de elementos utilizando foldl y foldr.
-- Ej: cantidadDeElementos [(8,6),(5,5),(5,6),(7,8)] = 4

                        	
-- Ejercicio 2: 
-- Dada una lista de pares (empleado, gasto), conocer el empleado más gastador usando foldl y foldr.
-- Ej: masGastador [("ana",8000),("pepe",4000),("juan",30000),("maria",12000)] = ("juan",30000)


-- Ejercicio 3: 
-- Dada una lista de (empleado, gasto), conocer el gasto total usando foldl y foldr.
-- Ej: monto  [("ana",8000),("pepe",4000),("juan",30000),("maria",12000)] = 54000


-- Ejercicio 4:
-- Completar con lo que corresponda para:
    -- > foldl …. 2 [(3+), (*2), (5+)] = 15

    -- > foldr …. 2 [(3+), (*2), (5+)] = 17


-- Ejercicio 5:
-- Dada una lista de proyectos

type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos :: [Proyecto]
proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 1000 ["cocinero"] ]

-- Determine una función que permita conocer el máximo proyecto según. Revolverlo usando foldl y foldr.
    -- La inversión inicial

    -- El nro de profesionales

    -- La cantidad de palabras del proyecto
    
