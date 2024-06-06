-- Un laboratorio está experimentando con distintos ratones para crear nuevos medicamentos naturales. 
-- De los ratones que usan, se conoce nombre, edad, peso, y las enfermedades que posee.

data Animal= Raton {nombre :: String, edad :: Double, peso :: Double, enfermedades :: [String]} deriving Show

-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]

-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = ["brucelosis", "tuberculosis"]

-- Ejercicio 1:
-- Hacer 4 funciones de modificación del ratón: 
    -- modificarNombre
    -- modificarEdad
    -- modificarPeso
    -- modificarEnfermedades
-- Deben recibir una función y un ratón, y devolver el ratón modificado. 


-- Ejercicio 2:
-- Existen distintos tipos de hierbas que afectan de diferentes maneras al ratón. Definir dichas hierbas:
    -- hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
    -- hierbaVerde, elimina una enfermedad dada.
    -- alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
    -- hierbaMagica, hace que el ratón pierda todas sus infecciones y quede con 0 años de edad.


-- Ejercicio 3:
-- Hacer la función medicamento, que recibe una lista de hierbas, un ratón, y administra al ratón todas las hierbas.  
    -- Hacer antiAge que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. 
        -- > antiAge (Raton "bicenterata" 256.0 0.2 [])
        -- Raton "bicenterata" 2.0 0.19 []

    -- Hacer reduceFatFast  (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” y tantas alcachofas como indique su potencia.  
        -- > reduceFatFast 1 (Raton “Orejudo" 4.0 10.0 ["obesidad", "sinusitis"])
	    -- Raton "Orejudo" 4.0 9.0 ["sinusitis"]
	
        -- > reduceFatFast 2 (Raton "Orejudo" 4.0 10.0 [“obesidad", "sinusitis"])
	    -- Raton "Orejudo" 4.0 8.1 ["sinusitis"]

    -- Hacer la función hierbaMilagrosa, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas.


-- Ejercicio 4:
-- Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar los resultados:
-- Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.
    -- > cantidadIdeal even
    -- 2

    -- > cantidadIdeal (>5)
    -- 6

-- Hacer la función estanMejoresQueNunca que dado un conjunto de ratones y un medicamento, es cierto cuando cada uno pesa menos de 1 kg después de aplicarle el medicamento dado.

-- Diseñar el siguiente experimento: dado un conjunto de ratones, encontrar la potencia ideal del reduceFatFast necesaria para que todos estén mejores que nunca.
