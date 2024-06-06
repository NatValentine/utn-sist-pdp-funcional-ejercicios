import Text.Show.Functions

-- Ejercicio 1:
-- Dado una lista de flores donde cada una está representada de la siguiente forma:
data Flor = Flor{nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Integer} deriving Show

rosa :: Flor
rosa = Flor "rosa" "decorativo" 120

jazmin :: Flor
jazmin =  Flor "jazmin" "aromatizante" 100

violeta :: Flor
violeta = Flor "violeta" "infusión" 110

orquidea :: Flor
orquidea =  Flor "orquidea" "decorativo" 90

flores :: [Flor]
flores = [orquidea, rosa,violeta, jazmin]

-- 1.1. Definir maximaFlorSegun que permite conocer el nombre de la flor que es máxima según estos criterios:
        -- La cantidad demandada
        -- La cantidad de letras de la flor
        -- El resto de la división de la cantidad demandada por 4
    -- Resolverla evitando tener código duplicado y usando recursividad.



-- 1.2. Dada una lista de flores determinar si están ordenadas de mayor a menor por cantidad de demanda.

-- Ejercicio 2:
-- Definir la función reversa de una lista
-- Ejemplos:
-- > reversa [“maria”, “juan”, “ana”]
-- > [“ana”, “juan”, “maria”]
--
-- > reversa [4,1,9,7]
-- > [7,9,1,4]


