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
modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre f unAnimal = unAnimal { nombre = (f.nombre)unAnimal}

    -- modificarEdad
modificarEdad :: (Double -> Double ) -> Animal -> Animal
modificarEdad f unAnimal = unAnimal {edad = (f.edad)unAnimal}

    -- modificarPeso
modificarPeso :: (Double -> Double) -> Animal -> Animal
modificarPeso f unAnimal = unAnimal {peso = (f.peso)unAnimal }

    -- modificarEnfermedades
modificarEnfermedades :: ([String] -> [String]) -> Animal -> Animal
modificarEnfermedades f unAnimal = unAnimal { enfermedades = (f.enfermedades)unAnimal}


-- Ejercicio 2:
-- Existen distintos tipos de hierbas que afectan de diferentes maneras al ratón. Definir dichas hierbas:
    -- hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
hierbaBuena :: Animal -> Animal
hierbaBuena unAnimal = modificarEdad sqrt unAnimal

    -- hierbaVerde, elimina una enfermedad dada.
hierbaVerde :: String -> Animal -> Animal
hierbaVerde unaEnfermedad unAnimal = modificarEnfermedades (filter (/= unaEnfermedad))  unAnimal

    -- alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
alcachofa :: Animal -> Animal
alcachofa unAnimal = modificarPeso  nuevoPeso unAnimal

nuevoPeso :: Double -> Double
nuevoPeso peso | peso > 2 = peso * 0.90
               | otherwise = peso * 0.95
    -- hierbaMagica, hace que el ratón pierda todas sus infecciones y quede con 0 años de edad.
hierbaMagica :: Animal -> Animal
hierbaMagica unAnimal = (modificarEdad (*0).modificarEnfermedad (const [])) unAnimal


-- Ejercicio 3:
-- Hacer la función medicamento, que recibe una lista de hierbas, un ratón, y administra al ratón todas las hierbas.  
medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento lista animal = foldl  (\unAni unaHierba -> unaHierba unAni)  animal  lista

medicamento' :: [(Animal -> Animal)] -> Animal -> Animal
medicamento' lista animal = foldl (flip ($)) animal lista

    -- Hacer antiAge que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. 
        -- > antiAge (Raton "bicenterata" 256.0 0.2 [])
        -- Raton "bicenterata" 2.0 0.19 []
antiAge :: Animal -> Animal
antiAge  = medicamento(replicate 3 hierbaBuena ++ [alcachofa]) 

    -- Hacer reduceFatFast  (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” y tantas alcachofas como indique su potencia.  
        -- > reduceFatFast 1 (Raton “Orejudo" 4.0 10.0 ["obesidad", "sinusitis"])
	    -- Raton "Orejudo" 4.0 9.0 ["sinusitis"]

        -- > reduceFatFast 2 (Raton "Orejudo" 4.0 10.0 [“obesidad", "sinusitis"])
	    -- Raton "Orejudo" 4.0 8.1 ["sinusitis"]
reduceFatFast :: Int -> Animal -> Animal
reduceFatFast unaPotencia unAnimal = medicamento ([hierbaVerde "obesidad"] ++ replicate unaPotencia alcachofa) unAnimal

    -- Hacer la función hierbaMilagrosa, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas.
hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa unAnimal = medicamento (map hierbaVerde enfermedadesInfecciosas) unAnimal


-- Ejercicio 4:
-- Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar los resultados:
-- Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.
    -- > cantidadIdeal even
    -- 2

    -- > cantidadIdeal (>5)
    -- 6
cantidadIdeal :: ( Int -> Bool)  -> Int
cantidadIdeal condicion = (head. filter condicion) [1..]

-- Hacer la función estanMejoresQueNunca que dado un conjunto de ratones y un medicamento, es cierto cuando cada uno pesa menos de 1 kg después de aplicarle el medicamento dado.
estanMejoresQueNunca :: [Animal]  -> (Animal -> Animal) -> Bool
estanMejoresQueNunca ratones medicamento = all ((<1).peso.medicamento) ratones

-- Diseñar el siguiente experimento: dado un conjunto de ratones, encontrar la potencia ideal del reduceFatFast necesaria para que todos estén mejores que nunca.
experimento :: [Animal] -> Int
experimento animales = cantidadIdeal(estanMejoresQueNunca animales.reduceFatFast)