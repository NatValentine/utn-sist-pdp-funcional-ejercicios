 import Text.Show.Functions

-- Es el momento de modelar las ciudades del pintoresco país de Haskellandia, de las que nos interesa conocer:
    -- su nombre
    -- el año de fundación
    -- las atracciones principales (como "Obelisco", "Pan de Azúcar", "El Gorosito", etc.)
    -- su costo de vida
data Ciudad = Ciudad {nombre :: String, fundacion :: Int, costoDeVida :: Int, atracciones :: [String]} deriving Show

baradero :: Ciudad
baradero = Ciudad "Baradero" 1615 150 ["Parque del Este", "Museo Alejandro Barbich"]

nullish :: Ciudad
nullish = Ciudad "Nullish" 1800 140 []

caletaOlivia :: Ciudad
caletaOlivia = Ciudad "Caleta Olivia" 1901 120 ["El Gorosito", "Faro Costanera"]

azul :: Ciudad
azul = Ciudad "Azul" 1832 190 ["Teatro Espaniol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]


-- Punto 1: Valor de una ciudad

-- 1.1. Definir el valor de una ciudad, un número que se obtiene de la siguiente manera:
    -- si fue fundada antes de 1800, su valor es 5 veces la diferencia entre 1800 y el año de fundación
    -- si no tiene atracciones, su valor es el doble del costo de vida
    -- de lo contrario, será 3 veces el costo de vida de la ciudad
valorDeCiudad :: Ciudad -> Int
valorDeCiudad ciudad | fundacion ciudad < 1800 = 5 * (1800 - fundacion ciudad)
                     | null (atracciones ciudad) = 2 * costoDeVida ciudad
                     | otherwise = 3 * costoDeVida ciudad

-- Ejemplos de invocación y respuesta
-- > Valor de la ciudad "Baradero", fundada en 1615, cuyas atracciones son "Parque del Este" y "Museo Alejandro Barbich", con un costo de vida de 150
-- 925

-- > Valor de la ciudad "Nullish", fundada en 1800, sin atracciones y un costo de vida de 140
-- 280

-- > Valor de la ciudad "Caleta Olivia", fundada en 1901, cuyas atracciones son "El Gorosito" y "Faro Costanera", y un costo de vida de 120
-- > 360


-- Punto 2: Características de las ciudades

-- 2.1. Alguna atracción copada:
-- Queremos saber si una ciudad tiene alguna atracción copada, esto es que la atracción comience con una vocal. 
-- Por ejemplo: "Acrópolis" es una atracción copada y "Golden Gate" no es copada.
-- En este caso puede utilizar la siguiente función:
isVowel :: Char -> Bool
isVowel character = character `elem` "aeiouAEIOU"

tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada = any (isVowel.head).atracciones

-- Ejemplos de invocación y respuesta:
-- Saber si la ciudad "6", fundada en 1615, cuyas atracciones son "Parque del Este" y "Museo Alejandro Barbich", con un costo de vida de 150 tiene alguna atracción copada
-- > Falso

-- Saber si la ciudad "Nullish", fundada en 1800, sin atracciones y un costo de vida de 140 tiene alguna atracción copada
-- > Falso

-- Saber si la ciudad "Caleta Olivia", fundada en 1901, cuyas atracciones son "El Gorosito" y "Faro Costanera", y un costo de vida de 120 tiene alguna atracción copada
-- > Verdadero


-- 2.2. Ciudad sobria:
-- Queremos saber si una ciudad es sobria, esto se da si todas las atracciones tienen más de x letras. El valor x tiene que poder configurarse.
ciudadSobria :: Int -> Ciudad -> Bool
ciudadSobria x ciudad | null (atracciones ciudad) = False
                      | otherwise = all (\a -> length a > x) (atracciones ciudad)

-- Ejemplos de invocación y respuesta:
-- > Saber si la ciudad "Baradero", fundada en 1615, cuyas atracciones son "Parque del Este" y "Museo Alejandro Barbich", con un costo de vida de 150 es sobria con atracciones de más de 14 letras
-- Verdadero ("Parque del Este" tiene 15 letras y el "Museo Alejandro Barbich" 24)

-- > Saber si la ciudad "Baradero", fundada en 1615, cuyas atracciones son "Parque del Este" y "Museo Alejandro Barbich", con un costo de vida de 150 es sobria con atracciones de más de 15 letras
-- Falso, porque "Parque del Este" tiene exactamente 15 letras y no más de 15.

-- > Saber si la ciudad "Nullish", fundada en 1800, sin atracciones y un costo de vida de 140 es sobria con 5 letras
-- Falso

-- 2.3. Ciudad con nombre raro:
-- Queremos saber si una ciudad tiene un nombre raro, esto implica que tiene menos de 5 letras en su nombre.
nombreRaro :: Ciudad -> Bool
nombreRaro = (< 5).length.nombre

-- Ejemplos de invocación y respuesta:
-- > Saber si la ciudad "Maipú", fundada en 1878, cuya atracción es el "Fortín Kakel" con un costo de vida de 115 tiene un nombre raro
-- Falso, "Maipú" tiene exactamente 5 letras, no menos

-- > Saber si la ciudad "Azul", fundada en 1832, cuyas atracciones son "Teatro Español", "Parque Municipal Sarmiento" y "Costanera Cacique Catriel", con un costo de vida de 190 tiene un nombre raro
-- Verdadero, "Azul" tiene 4 letras < 5


-- Punto 3: Eventos
-- Queremos poder registrar eventos que ocurren sobre una ciudad y que la afectan en mayor o menor medida. Dichos eventos son:

-- 3.1. Sumar una nueva atracción:
-- Queremos poder agregar una nueva atracción a la ciudad. Esto implica un esfuerzo de toda la comunidad en tiempo y dinero, lo que se traduce en un incremento del costo de vida de un 20%.
agregarAtraccion :: String -> Ciudad -> Ciudad
agregarAtraccion nuevaAtraccion ciudad = ciudad {
    atracciones = nuevaAtraccion : atracciones ciudad,
    costoDeVida = (costoDeVida ciudad * 6) `div` 5
}

-- Ejemplos de invocación y respuesta:
-- Agregar a la ciudad "Azul", fundada en 1832, cuyas atracciones son "Teatro Español", "Parque Municipal Sarmiento" y "Costanera Cacique Catriel", con un costo de vida de 190, una nueva atracción: "Balneario Municipal Alte. Guillermo Brown".
-- La ciudad debe tener 4 atracciones y su costo de vida debe quedar en 228 (190 + 20%)


-- 3.2. Crisis:
-- Al atravesar una crisis, la ciudad baja un 10% su costo de vida y se debe cerrar la última atracción de la lista.
crisis :: Ciudad -> Ciudad
crisis ciudad = ciudad {
    atracciones = init (atracciones ciudad),
    costoDeVida = (costoDeVida ciudad * 9) `div` 10
}

-- Ejemplos de invocación y respuesta:
-- Que haya una crisis sobre la ciudad "Azul", fundada en 1832, cuyas atracciones son "Teatro Español", "Parque Municipal Sarmiento" y "Costanera Cacique Catriel", con un costo de vida de 190.
-- > La ciudad debe tener 2 atracciones: "Teatro Español" y "Parque Municipal Sarmiento" y un costo de vida de 171 (10% menos de 190)
-- Que haya una crisis sobre ciudad "Nullish", fundada en 1800, sin atracciones y un costo de vida de 140
-- > La ciudad debe tener un costo de vida de 126 (10% menos de 140) y no tener atracciones.


-- 3.3. Remodelación
-- Al remodelar una ciudad, incrementa su costo de vida un porcentaje que se indica al hacer la remodelación y le agrega el prefijo "New " al nombre.
remodelacion :: Int -> Ciudad -> Ciudad
remodelacion porcentaje ciudad = ajustarCostoDeVida porcentaje ciudad {
    nombre = "New " ++ nombre ciudad
}

ajustarCostoDeVida :: Int -> Ciudad -> Ciudad
ajustarCostoDeVida porcentaje ciudad = ciudad {
    costoDeVida = (costoDeVida ciudad * (100 + porcentaje)) `div` 100
}

-- Ejemplos de invocación y respuesta:
-- Que haya una remodelación al 50% sobre la ciudad "Azul", fundada en 1832, cuyas atracciones son "Teatro Español", "Parque Municipal Sarmiento" y "Costanera Cacique Catriel", con un costo de vida de 190.
-- > La ciudad debe tener un nombre de "New Azul" y su costo de vida debe aumentar a 285.


-- 3.4. Reevaluación:
-- Si la ciudad es sobria con atracciones de más de n letras (valor que se quiere configurar), aumenta el costo de vida un 10%, si no baja 3 puntos.
reevaluacion :: Int -> Ciudad -> Ciudad
reevaluacion x ciudad | ciudadSobria x ciudad = ajustarCostoDeVida 10 ciudad
                      | otherwise = ciudad { costoDeVida = costoDeVida ciudad - 3 }

-- Ejemplos de invocación y respuesta:
-- Que haya una reevaluación de atracciones de 14 letras sobre la ciudad "Azul", fundada en 1832, cuyas atracciones son "Teatro Español", "Parque Municipal Sarmiento" y "Costanera Cacique Catriel", con un costo de vida de 190.
-- Dado que el "Teatro Español" tiene 14 letras (no más), la ciudad no es sobria entonces baja a un costo de vida de 187 puntos
-- Que haya una reevaluación de atracciones de 13 letras sobre la ciudad "Azul", fundada en 1832, cuyas atracciones son "Teatro Español", "Parque Municipal Sarmiento" y "Costanera Cacique Catriel", con un costo de vida de 190.
-- En este caso la ciudad es sobria, por lo que aumenta su costo de vida a 209 (190 + 10% = 19)


-- Punto 4: La transformación no para
-- Reflejar, en la consola de GHCi, de qué manera podemos hacer que una ciudad tenga:
    -- el agregado de una nueva atracción
    -- una remodelación
    -- una crisis
    -- una reevaluación
data ParametrosTransformacion = ParametrosTransformacion {
    setNuevaAtraccion :: String,
    setPorcentajeRemodelacion :: Int,
    setReevaluacionParametro :: Int
}

parametrosPorDefecto :: ParametrosTransformacion
parametrosPorDefecto = ParametrosTransformacion {
    setNuevaAtraccion = "Balneario Municipal Alte. Guillermo Brown",
    setPorcentajeRemodelacion = 50,
    setReevaluacionParametro = 14
}

transformarCiudadConParametros :: ParametrosTransformacion -> Ciudad -> Ciudad
transformarCiudadConParametros parametros ciudad =
    reevaluacion (setReevaluacionParametro parametros) . remodelacion (setPorcentajeRemodelacion parametros) . crisis . agregarAtraccion (setNuevaAtraccion parametros) $ ciudad

transformarCiudad :: Ciudad -> Ciudad
transformarCiudad = transformarCiudadConParametros parametrosPorDefecto

-- > transformarCiudad caletaOlivia
-- Ciudad {nombre = "New Caleta Olivia", fundacion = 1901, costoDeVida = 190, atracciones = ["Balneario Municipal Alte. Guillermo Brown","El Gorosito"]}

-- > transformarCiudad azul        
-- Ciudad {nombre = "New Azul", fundacion = 1832, costoDeVida = 337, atracciones = ["Balneario Municipal Alte. Guillermo Brown","Teatro Espaniol","Parque Municipal Sarmiento"]}


-- Punto 5 - Un año para recordar
-- 5.1. Los años pasan... Queremos modelar un año, donde definamos
    -- el número que le corresponde
data Anio = Anio {numeroAnio :: Int, eventos :: [Evento]}

    -- una serie de eventos que se produjeron
data Evento = Evento {nombreEvento :: String, aplicarEvento :: Ciudad -> Ciudad}

-- También queremos reflejar el paso de un año para una ciudad, es decir, que los eventos afecten el estado final en el que queda una ciudad.
pasoDelAnio :: Anio -> Ciudad -> Ciudad
pasoDelAnio anio ciudad = foldl (flip aplicarEvento) ciudad (eventos anio)

-- Ejemplos de invocación y respuesta:
-- Reflejar para Azul el paso del año 2022 que tiene como eventos: una crisis, una remodelación de 5% y una reevaluación de 7 letras para las atracciones.
-- > Debe quedar con el nombre "New Azul",el costo de vida de 197.505 y las atracciones "Teatro Español" y "Parque Municipal Sarmiento"

-- Reflejar para Azul el paso del año 2015 que no tiene eventos.
-- >Debe quedar con el mismo costo de vida.

--5.2.  Algo mejor
-- Implementar una función que reciba una ciudad, un criterio de comparación y un evento, de manera que nos diga si la ciudad tras el evento subió respecto a ese criterio. 
ciudadMejorSegun :: Ord a => (Ciudad -> a) -> Evento -> Ciudad -> Bool
ciudadMejorSegun criterio evento ciudad = criterio (aplicarEvento evento ciudad) > criterio ciudad

--Ejemplos de invocación y respuesta:
-- Para la ciudad Azul, el evento crisis y el criterio para comparar el costoDeVida.
-- >Falso (ya que al aplicar el evento crisis sobre Azul, el costo de vida bajó).

-- Para la ciudad Azul, el evento Agregar atracción "Monasterio Trapense" y el criterio para comparar el costo de vida.
-- > Verdadero (el costo de vida subió un 20%)

-- Para la ciudad Azul, el evento Agregar atracción "Monasterio Trapense" y el criterio para comparar la cantidad de atracciones.
-- > Verdadero (pasó de 2 a 3 atracciones)


--5.3. Costo de vida que suba
-- Para un año, queremos aplicar sobre una ciudad solo los eventos que hagan que el costo de vida suba. Debe quedar como resultado la ciudad afectada con dichos eventos.
aplicarEventosQueSubenCosto :: [Evento] -> Ciudad -> Ciudad
aplicarEventosQueSubenCosto [] ciudad = ciudad
aplicarEventosQueSubenCosto (ev:evs) ciudad | costoDeVida (aplicarEvento ev ciudad) > costoDeVida ciudad = aplicarEventosQueSubenCosto resto (aplicarEvento ev ciudad)
                                            | otherwise = aplicarEventosQueSubenCosto evs ciudad

-- Ejemplos de invocación y respuesta:
-- Aplicar el año 2022 (del punto 1.1) sobre la ciudad Azul
-- > Debe quedar con el nombre "New Azul", y un costo de vida de 219.45 


-- 5.4 Costo de vida que baje
-- Para un año, queremos aplicar solo los eventos que hagan que el costo de vida baje. Debe quedar como resultado la ciudad afectada con dichos eventos.
aplicarEventosQueBajanCosto :: [Evento] -> Ciudad -> Ciudad
aplicarEventosQueBajanCosto [] ciudad = ciudad
aplicarEventosQueBajanCosto (ev:evs) ciudad | costoDeVida (aplicarEvento ev ciudad) < costoDeVida ciudad = aplicarEventosQueBajanCosto evs (aplicarEvento ev ciudad)
                                            | otherwise = aplicarEventosQueBajanCosto evs ciudad

-- Ejemplos de invocación y respuesta
-- Aplicar el año 2022 (del punto 1.1) sobre la ciudad Azul
-- > Debe quedar con el mismo nombre ("Azul"), un costo de 171 y perder la última atracción


-- 5.5. Valor que suba
-- Para un año, queremos aplicar solo los eventos que hagan que el valor suba. Debe quedar como resultado la ciudad afectada con dichos eventos.
valorCiudad :: Ciudad -> Int
valorCiudad ciudad | fundacion ciudad < 1800 = fromIntegral (5 * (1800 - fundacion ciudad))
                   | null (atracciones ciudad) = 2 * costoDeVida ciudad
                   | otherwise = 3 * costoDeVida ciudad

aplicarEventosQueSubenValor :: [Evento] -> Ciudad -> Ciudad
aplicarEventosQueSubenValor [] ciudad = ciudad
aplicarEventosQueSubenValor (ev:evs) ciudad | valorCiudad (aplicarEvento ev ciudad) > valorCiudad ciudad = aplicarEventosQueSubenValor evs (aplicarEvento ev ciudad)
                                            | otherwise = aplicarEventosQueSubenValor evs ciudad

-- Ejemplos de invocación y respuesta:
-- Aplicar el año 2022 (del punto 5.1) sobre la ciudad Nullish
-- > Debe quedar con el nombre "New Nullish" y el costo de vida 147


-- Punto 6: Funciones a la orden

-- 6.1 Eventos ordenados
-- Dado un año y una ciudad, queremos saber si los eventos están ordenados en forma correcta, esto implica que el costo de vida al aplicar cada evento se va incrementando respecto al anterior evento. Debe haber al menos un evento para dicho año.
eventosOrdenados :: [Evento] -> Ciudad -> Bool
eventosOrdenados [evento] ciudad = True
eventosOrdenados (evento1:evento2:resto) ciudad =
    costoDeVida (aplicarEvento evento2 (aplicarEvento evento1 ciudad)) > costoDeVida (aplicarEvento evento1 ciudad) 
    && eventosOrdenados (evento2:resto) (aplicarEvento evento1 ciudad)

-- Ejemplos de invocación y respuesta:
-- Saber si el año 2022 (4.1) sobre Azul tiene los eventos ordenados
-- > Verdadero

-- Saber si el año 2023 que tiene los siguientes eventos: crisis, agregar como atracción "parque", hacer una remodelación al 10%, y hacer una remodelación al 20%, sobre Azul tiene los eventos ordenados
-- > Falso, al agregar la atracción le sube un 20% el costo de vida y la remodelación un 10%, por lo tanto no está ordenado.


-- 6.2 Ciudades ordenadas
-- Dado un evento y una lista de ciudades, queremos saber si esa lista está ordenada. Esto implica que el costo de vida al aplicar el evento sobre cada una de las ciudades queda en orden creciente. Debe haber al menos una ciudad en la lista.
ciudadesOrdenadas :: (Ciudad -> Ciudad) -> [Ciudad] -> Bool
ciudadesOrdenadas evento [ciudad] = True
ciudadesOrdenadas evento (ciudad1:ciudad2:resto) =
    costoDeVida (evento ciudad2) >= costoDeVida (evento ciudad1) 
    && ciudadesOrdenadas evento (ciudad2:resto)

-- Ejemplos de invocación y respuesta:
-- Saber si para el evento remodelación al 10% de las ciudades Caleta Olivia, Nullish, Baradero y Azul está ordenada
-- > Verdadero

-- Saber si para el evento remodelación al 10% de las ciudades Caleta Olivia, Azul y Baradero está ordenada
-- > Falso


-- 6.3 Años ordenados
-- Dada una lista de años y una ciudad, queremos saber si el costo de vida al aplicar todos los eventos de cada año sobre esa ciudad termina generando una serie de costos de vida ascendente (de menor a mayor). Debe haber al menos un año en la lista.
aniosOrdenados :: [Anio] -> Ciudad -> Bool
aniosOrdenados [anio] ciudad = True
aniosOrdenados (anio1:anio2:resto) ciudad =
    costoDeVida (aplicarEventosAnio anio2 (aplicarEventosAnio anio1 ciudad)) > costoDeVida (aplicarEventosAnio anio1 ciudad) 
    && aniosOrdenados (anio2:resto) (aplicarEventosAnio anio1 ciudad)

aplicarEventosAnio :: Anio -> Ciudad -> Ciudad
aplicarEventosAnio anio ciudad = foldl (flip aplicarEvento) ciudad(eventos anio)

-- Tenemos los años:
    -- 2021 que tiene los siguientes eventos: crisis y agregar como atracción "playa"
    -- 2022 que está definido en el punto 1.1 (tiene como eventos: una crisis, una remodelación de 5% y una reevaluación de 7 letras para las atracciones)
    -- 2023 que tiene los siguientes eventos: crisis, agregar como atracción "parque", hacer una remodelación al 10%, y hacer una remodelación al 20%

-- Ejemplos de invocación y respuesta
-- Que los años 2021, 2022 y 2023 estén ordenados para Baradero
-- > Falso

-- Que los años 2022, 2021 y 2023 estén ordenados para Baradero
-- >Verdadero, al aplicar los años en ese orden va subiendo el costo de vida.


-- Punto 7: Al infinito, y más allá...
-- 7.1. Eventos ordenados
-- Definir el año 2024 con una lista de eventos que inicia con una crisis, luego una reevaluación de atracciones con 7 letras y luego tiene una sucesión infinita de remodelaciones cuyo porcentaje de aumento en el costo de vida es 1 para la primera remodelación, 2 para la siguiente, y así hasta el infinito.
-- Puede haber un resultado posible para la función del punto 5.1 (eventos ordenados) para el año 2024? 
-- Justificarlo relacionándolo con conceptos vistos en la materia.

-- 7.2. Ciudades ordenadas
-- Puede haber un resultado posible para la función del punto 2.2 (ciudades ordenadas) para la lista "disco rayado"? 
-- Justificarlo relacionándolo con conceptos vistos en la materia.
-- Definir una lista de ciudades "disco rayado" que comience con Azul y Nullish y luego cicle intercalando infinitamente entre Caleta Olivia y Baradero.

-- 7.2. Años ordenados
-- Puede haber un resultado posible para la función del punto 2.3 (años ordenados) para una lista infinita de años? 
-- Justificarlo relacionándolo con conceptos vistos en la materia.
