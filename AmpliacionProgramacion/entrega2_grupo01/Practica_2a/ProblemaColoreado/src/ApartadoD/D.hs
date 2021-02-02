{--
	Fichero: D.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 30/10/2020
	Funcionalidad: Implementacion del apartado d de la P2a.
--}

{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoD.D where

import Data.List
import Control.Monad

-- Cosas para dibujas
type Mosaico = [String]

-- Mapa de 11x24 
altura :: Int
altura = 11
anchura :: Int
anchura = 24

altura' :: Integer
altura' = 11
anchura' :: Integer
anchura' = 24

-- Mosaico inicial
mosaicoInicial :: Mosaico
mosaicoInicial = replicate altura (replicate anchura '.')

-- Funcion que dibuja un Mosaico
dibujarMosaico :: Mosaico -> IO ()
dibujarMosaico = putStr . unlines -- pointfree

-- Tipo Color
data Color = Rojo | Verde | Azul | Naranja | Morado deriving (Show,Enum,Eq)

-- Tipo Provincia
data Provincia = Provincia {arribaIzq::(Integer, Integer), abajoDer::(Integer, Integer)}
    deriving (Show,Eq)

-- Tipo Frontera
type Frontera = Provincia -> [Provincia]

-------------------------------------------------------------------------------------------
-- APARTADO C
-------------------------------------------------------------------------------------------
-- Mensaje de error por coordenadas en variable global y solapamiento
msg :: String
msg = "ERROR, valores incorrectos para coordenadas"
msg' :: String
msg' = "ERROR, existe solapamiento entre las provincias"

-- Funciones que comprueban si la x y la y se encuentran dentro de los parametros posibles
comprobarX :: Integer -> Integer -> Bool
comprobarX x x' = not ((x<0) || (x>anchura') || (x>x'))
comprobarY :: Integer -> Integer -> Bool
comprobarY y y' = not ((y<0) || (y>altura') || (y>y'))

-- Funcion que comprueba si los parametros de la nueva provincia estan bien y en funcion de eso se crea o no
crearProvincia :: (Integer, Integer) -> (Integer, Integer) -> Either String Provincia
crearProvincia (x1, y1) (x2, y2) = do let c1 = comprobarX x1 x2
                                          c2 = comprobarY y1 y2
                                      if not (c1&&c2) then
                                          Left msg
                                      else
                                          return (Provincia (x1,y1) (x2,y2))

-- Funcion que convierte un Right Provincia en un Provincia
convierteProvincia :: Either String Provincia -> Provincia
convierteProvincia (Right (Provincia (x1,y1) (x2,y2))) = Provincia (x1,y1) (x2,y2)

-- Funcion que genera un rectangulo como area
generaRectangulo :: (Integer, Integer) -> (Integer, Integer) -> Integer -> [Integer]
generaRectangulo (x1, y1) (x2, y2) x
                                        | y1 > y2 = []
                                        | x1 == x2 = (x1+(anchura'*(y1-1))) : generaRectangulo (x, y1+1) (x2, y2) x
                                        | otherwise = (x1+(anchura'*(y1-1))) : generaRectangulo (x1+1, y1) (x2, y2) x

-- Funcion que indica si dos provincias son vecinas
sonVecinos' :: Provincia -> Provincia -> Either String Bool
sonVecinos' (Provincia (x11, y11) (x12, y12)) (Provincia (x21, y21) (x22, y22))
    | (x11 == x21) && (x12 == x22) && (y11 == y21) && (y12 == y22) = Left msg'
    | not (null (generaRectangulo (x11, y11) (x12, y12) x11 `intersect` generaRectangulo (x21, y21) (x22, y22) x21)) = Left msg'
    | (x11 == (x22 + 1) || x12 == (x21 - 1) || x11 == (x22 - 1) || x12 == (x21 + 1)) && not (null ([y11 .. y12] `intersect` [y21 .. y22])) = 
        return True
    | (y11 == (y22 + 1) || y12 == (y21 - 1) || y11 == (y22 - 1) || y12 == (y21 + 1)) && not (null ([x11 .. x12] `intersect` [x21 .. x22])) =
        return True
    | otherwise = return False

-- Funcion que saca provincias de una lista de Either de provincias
sacarProvincia :: Either String [Provincia] -> [Provincia]
sacarProvincia (Left _) = []
sacarProvincia (Right []) = []
sacarProvincia (Right [x]) = [x]
sacarProvincia (Right (x:xs)) = x:xs

-- Funcion que devuelve una lista de fronteras para una lista de provincias
fronteras' :: Provincia -> [Provincia] -> Either String [Provincia]
fronteras' _ [] = Right []
fronteras' provincia [x] = do let vfront = sonVecinos' provincia x
                              if provincia == x then
                                  return []
                              else if vfront == Left msg' then
                                  Left msg'
                              else if vfront == Right True then
                                  return [x]
                              else
                                  return []
fronteras' provincia (x:xs) = if provincia == x then
                                    fronteras' provincia xs else
                              do let vfront = sonVecinos' provincia x 
                                     vfront' = fronteras' provincia xs
                                 if (vfront == Left msg') || (vfront' == Left msg') then
                                     Left msg'
                                 else if vfront == Right True then
                                     return (x : sacarProvincia vfront')
                                 else
                                     return (sacarProvincia vfront')

-- Funcion que ayuda a comprobar las provincias con >>=
compruebaProvincia :: Provincia -> Either String Provincia
compruebaProvincia = Right

-- Funcion que comprueba que no hay errores y se puede sacar el mapa
compruebaProvincias :: [Either String Provincia] -> Bool
compruebaProvincias [] = True
compruebaProvincias [x] = do let comp = x >>= compruebaProvincia
                             comp /= Left msg

compruebaProvincias (x:xs) = do let res = Left msg `notElem` (x:xs)
                                res

-- Funcion que comprueba las fronteras
compruebaFronteras :: Either String [Provincia] -> Bool
compruebaFronteras (Left _) = False
compruebaFronteras (Right []) = True
compruebaFronteras (Right [x]) = True
compruebaFronteras (Right (x:xs)) = Left msg' `notElem` [fronteras' x' (x:xs) | x' <- x:xs]

-- Funcion que convierte una lista de Either a una lista Either de Provincias
convierteListaProvincias :: [Either String Provincia] -> Either String [Provincia]
convierteListaProvincias [] = Right []
convierteListaProvincias [x] = Right [convierteProvincia x]
convierteListaProvincias (x:xs) = Right [convierteProvincia l | l <- x:xs]


-- Funcion que saca las fronteras de una provincia
fr :: [Provincia] -> Frontera
fr (x:xs) p = sacarProvincia (fronteras' p (x:xs))

-- Tipo Mapa
data Mapa = Atlas [Provincia] Frontera

-- Mapa con provincias
mapaAux :: [Provincia] -> Mapa
mapaAux (x:xs) = Atlas (x:xs) (fr (x:xs))
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- Colores de provincias vecinas para un coloreado
coloresFrontera ::
    Provincia->[(Provincia,Color)]->Frontera-> [Color]
coloresFrontera provincia coloreado frontera
    = [col | (prov,col)<- coloreado, prov `elem` frontera provincia]

-- Posibles coloreados para un mapa y una lista de colores
coloreados :: (Mapa,[Color]) -> [[(Provincia,Color)]]
coloreados (Atlas [] _, _) = [[]]
coloreados (Atlas (prov:provs) frontera, colores)
    = [(prov,color):coloreado' |
        coloreado' <- coloreados
            (Atlas provs frontera, colores)
        , color <- colores \\
            coloresFrontera prov coloreado' frontera]

-- Funcion que incluye las provincias en el mosaico
incluirProvincias :: Mosaico -> [(Provincia,Color)] -> Mosaico
incluirProvincias = foldl incluirProvincia  -- incluirProvincia es la funcion de pliegue

-- Funcion que asigna una letra a cada color
getLetra :: Color -> Char
getLetra Rojo = 'r'
getLetra Verde = 'v'
getLetra Azul = 'a'
getLetra Naranja = 'n'
getLetra Morado = 'm'


-- Operador para sacar posicion usando Integer
(!!!) :: [a] -> Integer -> a
(x:xs) !!! n
        | n < 0 = error "Posicion Negativa"
        | n == 0 = x
        | otherwise = xs !!! (n-1)

-- Superpone el cuadrado en las posiciones del mosaico
incluirProvincia :: Mosaico -> (Provincia,Color) -> Mosaico
incluirProvincia mosaico (provincia,color)  = map fila mapa_altura 
        where mapa_altura :: [Integer] 
              mapa_altura = [1..altura']
              fila n = map (letra n) mapa_anchura 
              mapa_anchura :: [Integer]
              mapa_anchura = [1..anchura']
              (letra n) m | dentroProvincia n m provincia = getLetra color
                          | otherwise = mosaico !!! n' !!! m' 
                            where n' = n-1
                                  m' = m-1

-- Funcion que comprueba si una provincia esta dentro del mapa o no
dentroProvincia :: Integer -> Integer -> Provincia -> Bool
dentroProvincia n m provincia =
        (y2 >= n)  && (x1 <= m)
        && (y1 <= n) && (x2 >= m)
        where (x1, y1) = arribaIzq provincia
              (x2, y2) = abajoDer provincia

-- Solucion al problema de los colores
solucionColorear:: (Mapa,[Color]) -> [(Provincia,Color)]
solucionColorear (m, color) = let sol' = coloreados (m, color) in
                                if sol' /= [] then head sol' else []

---------------------------------------------------------------------------------------------
-- APARTADO D
---------------------------------------------------------------------------------------------
-- Funcion que va pidiendo colores y devuelve una lista con los colores introducidos
introducirColores :: [Color] -> IO [Color]
introducirColores [] = do putStrLn "¿Desea introducir un nuevo color? (S):"
                          introducirCol <- getLine
                          if introducirCol == "S" then
                             do putStrLn "Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:"
                                nuevoColor <- getLine
                                if nuevoColor == "Rojo" then
                                    do let nuevoColor' = Rojo
                                       introducirColores [nuevoColor']
                                else if nuevoColor == "Verde" then
                                    do let nuevoColor' = Verde
                                       introducirColores [nuevoColor']
                                else if nuevoColor == "Azul" then
                                    do let nuevoColor' = Azul
                                       introducirColores [nuevoColor']
                                else if nuevoColor == "Naranja" then
                                    do let nuevoColor' = Naranja
                                       introducirColores [nuevoColor']
                                else if nuevoColor == "Morado" then
                                    do let nuevoColor' = Morado
                                       introducirColores [nuevoColor']
                                else
                                    do putStrLn "Color incorrecto. Intentalo de nuevo."
                                       introducirColores []
                          else
                              do putStrLn "Debe introducir al menos un color."
                                 introducirColores []
introducirColores [colores] = do putStrLn "¿Desea introducir un nuevo color? (S):"
                                 introducirCol <- getLine
                                 if introducirCol == "S" then
                                    do putStrLn "Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:"
                                       nuevoColor <- getLine
                                       if nuevoColor == "Rojo" then
                                           do let nuevoColor' = Rojo
                                              if nuevoColor' `notElem` [colores] then
                                                  introducirColores [colores, nuevoColor']
                                              else
                                                  do putStrLn "Color repetido. Intentalo de nuevo."
                                                     introducirColores [colores]
                                       else if nuevoColor == "Verde" then
                                           do let nuevoColor' = Verde
                                              if nuevoColor' `notElem` [colores] then
                                                  introducirColores [colores, nuevoColor']
                                              else
                                                  do putStrLn "Color repetido. Intentalo de nuevo."
                                                     introducirColores [colores]
                                       else if nuevoColor == "Azul" then
                                           do let nuevoColor' = Azul
                                              if nuevoColor' `notElem` [colores] then
                                                  introducirColores [colores, nuevoColor']
                                              else
                                                  do putStrLn "Color repetido. Intentalo de nuevo."
                                                     introducirColores [colores]
                                       else if nuevoColor == "Naranja" then
                                           do let nuevoColor' = Naranja
                                              if nuevoColor' `notElem` [colores] then
                                                  introducirColores [colores, nuevoColor']
                                              else
                                                  do putStrLn "Color repetido. Intentalo de nuevo."
                                                     introducirColores [colores]
                                       else if nuevoColor == "Morado" then
                                           do let nuevoColor' = Morado
                                              if nuevoColor' `notElem` [colores] then
                                                  introducirColores [colores, nuevoColor']
                                              else
                                                  do putStrLn "Color repetido. Intentalo de nuevo."
                                                     introducirColores [colores]
                                       else
                                           do putStrLn "Color incorrecto. Intentalo de nuevo."
                                              introducirColores [colores]
                                 else
                                     return [colores]
introducirColores (c:cs) = do putStrLn "¿Desea introducir un nuevo color? (S):"
                              introducirCol <- getLine
                              if introducirCol == "S" then
                                 do putStrLn "Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:"
                                    nuevoColor <- getLine
                                    if nuevoColor == "Rojo" then
                                       do let nuevoColor' = Rojo
                                          if nuevoColor' `notElem` (c:cs) then
                                              introducirColores (nuevoColor':(c:cs))
                                          else
                                              do putStrLn "Color repetido. Intentalo de nuevo."
                                                 introducirColores (c:cs)
                                    else if nuevoColor == "Verde" then
                                       do let nuevoColor' = Verde
                                          if nuevoColor' `notElem` (c:cs) then
                                              introducirColores (nuevoColor':(c:cs))
                                          else
                                              do putStrLn "Color repetido. Intentalo de nuevo."
                                                 introducirColores (c:cs)
                                    else if nuevoColor == "Azul" then
                                       do let nuevoColor' = Azul
                                          if nuevoColor' `notElem` (c:cs) then
                                              introducirColores (nuevoColor':(c:cs))
                                          else
                                              do putStrLn "Color repetido. Intentalo de nuevo."
                                                 introducirColores (c:cs)
                                    else if nuevoColor == "Naranja" then
                                       do let nuevoColor' = Naranja
                                          if nuevoColor' `notElem` (c:cs) then
                                              introducirColores (nuevoColor':(c:cs))
                                          else
                                              do putStrLn "Color repetido. Intentalo de nuevo."
                                                 introducirColores (c:cs)
                                    else if nuevoColor == "Morado" then
                                       do let nuevoColor' = Morado
                                          if nuevoColor' `notElem` (c:cs) then
                                              introducirColores (nuevoColor':(c:cs))
                                          else
                                              do putStrLn "Color repetido. Intentalo de nuevo."
                                                 introducirColores (c:cs)
                                    else
                                       do putStrLn "Color incorrecto. Intentalo de nuevo."
                                          introducirColores (c:cs)
                              else
                                 return (c:cs)

-- Funcion que va pidiendo datos por pantalla y muestra la solucion al problema del coloreado
introducirRectangulo :: [Either String Provincia] -> IO ()
introducirRectangulo [] = do putStrLn "¿Desea introducir un nuevo rectángulo? (S):"
                             introducirRect <- getLine
                             Control.Monad.when (introducirRect == "S") $
                                 do putStrLn "Introduzca la coordenada x1 (x superior izquierda):"
                                    x1 <- getLine
                                    putStrLn "Introduzca la coordenada y1 (y superior izquierda):"
                                    y1 <- getLine
                                    putStrLn "Introduzca la coordenada x2 (x inferior derecha):"
                                    x2 <- getLine
                                    putStrLn "Introduzca la coordenada y2 (y inferior derecha):"
                                    y2 <- getLine
                                    let x1' = read x1 :: Integer
                                        y1' = read y1 :: Integer
                                        x2' = read x2 :: Integer
                                        y2' = read y2 :: Integer
                                        nuevoRectangulo = crearProvincia (x1',y1') (x2',y2')
                                        lista1 = [nuevoRectangulo]
                                        listaComprueba = compruebaProvincias lista1
                                    if not listaComprueba then
                                        do putStrLn msg
                                           introducirRectangulo []
                                    else
                                        do let lista2 = convierteListaProvincias lista1
                                               listaComprueba2 = compruebaFronteras lista2
                                           if listaComprueba2 then
                                                do colores <- introducirColores []
                                                   let mapa1 = mapaAux (sacarProvincia lista2)
                                                       sol1 = solucionColorear (mapa1, colores)
                                                   dibujarMosaico (incluirProvincias mosaicoInicial sol1)
                                                   introducirRectangulo lista1
                                            else
                                                do putStrLn msg'
                                                   introducirRectangulo []
introducirRectangulo [listaRectangulos] = do putStrLn "¿Desea introducir un nuevo rectángulo? (S):"
                                             introducirRect <- getLine
                                             Control.Monad.when (introducirRect == "S") $
                                                 do putStrLn "Introduzca la coordenada x1 (x superior izquierda):"
                                                    x1 <- getLine
                                                    putStrLn "Introduzca la coordenada y1 (y superior izquierda):"
                                                    y1 <- getLine
                                                    putStrLn "Introduzca la coordenada x2 (x inferior derecha):"
                                                    x2 <- getLine
                                                    putStrLn "Introduzca la coordenada y2 (y inferior derecha):"
                                                    y2 <- getLine
                                                    let x1' = read x1 :: Integer
                                                        y1' = read y1 :: Integer
                                                        x2' = read x2 :: Integer
                                                        y2' = read y2 :: Integer
                                                        nuevoRectangulo = crearProvincia (x1',y1') (x2',y2')
                                                        lista1 = [listaRectangulos, nuevoRectangulo]
                                                        listaComprueba = compruebaProvincias lista1
                                                    if not listaComprueba then
                                                        do putStrLn msg
                                                           introducirRectangulo [listaRectangulos]
                                                    else
                                                        do let lista2 = convierteListaProvincias lista1
                                                               listaComprueba2 = compruebaFronteras lista2
                                                           if listaComprueba2 then
                                                                do colores <- introducirColores []
                                                                   let mapa1 = mapaAux (sacarProvincia lista2)
                                                                       sol1 = solucionColorear (mapa1, colores)
                                                                   if sol1 /= [] then
                                                                       do dibujarMosaico (incluirProvincias mosaicoInicial sol1)
                                                                          introducirRectangulo lista1
                                                                   else
                                                                       do putStrLn "No hay solucion."
                                                                          introducirRectangulo lista1
                                                            else
                                                                do putStrLn msg'
                                                                   introducirRectangulo [listaRectangulos]
introducirRectangulo (r:rs) = do putStrLn "¿Desea introducir un nuevo rectángulo? (S):"
                                 introducirRect <- getLine
                                 Control.Monad.when (introducirRect == "S") $
                                        do putStrLn "Introduzca la coordenada x1 (x superior izquierda):"
                                           x1 <- getLine
                                           putStrLn "Introduzca la coordenada y1 (y superior izquierda):"
                                           y1 <- getLine
                                           putStrLn "Introduzca la coordenada x2 (x inferior derecha):"
                                           x2 <- getLine
                                           putStrLn "Introduzca la coordenada y2 (y inferior derecha):"
                                           y2 <- getLine
                                           let x1' = read x1 :: Integer
                                               y1' = read y1 :: Integer
                                               x2' = read x2 :: Integer
                                               y2' = read y2 :: Integer
                                               nuevoRectangulo = crearProvincia (x1',y1') (x2',y2')
                                               lista1 = nuevoRectangulo:r:rs
                                               listaComprueba = compruebaProvincias lista1
                                           if not listaComprueba then
                                                do putStrLn msg
                                                   introducirRectangulo (r:rs)
                                           else
                                                do let lista2 = convierteListaProvincias lista1
                                                       listaComprueba2 = compruebaFronteras lista2
                                                   if listaComprueba2 then
                                                       do colores <- introducirColores []
                                                          let mapa1 = mapaAux (sacarProvincia lista2)
                                                              sol1 = solucionColorear (mapa1, colores)
                                                          if sol1 /= [] then
                                                                do dibujarMosaico (incluirProvincias mosaicoInicial sol1)
                                                                   introducirRectangulo lista1
                                                          else
                                                                do putStrLn "No hay solucion."
                                                                   introducirRectangulo lista1
                                                   else
                                                       do putStrLn msg'
                                                          introducirRectangulo (r:rs)

-- Main para el apartado D 
mainApartadoD :: IO ()
mainApartadoD = introducirRectangulo []                   

{--
    Resultado esperado (ESTOS DOS EJEMPLOS SON ORIENTATIVOS YA QUE ES EL USUARIO EL QUE INTRODUCE LOS DATOS POR TECLADO):

    EJEMPLO 1:
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    1
    Introduzca la coordenada y1 (y superior izquierda):
    2
    Introduzca la coordenada x2 (x inferior derecha):
    3
    Introduzca la coordenada y2 (y inferior derecha):
    4
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Verde
    ¿Desea introducir un nuevo color? (S):
    S 
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Morado
    ¿Desea introducir un nuevo color? (S):

    ........................
    vvv.....................
    vvv.....................
    vvv.....................
    ........................
    ........................
    ........................
    ........................
    ........................
    ........................
    ........................
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    1
    Introduzca la coordenada y1 (y superior izquierda):
    2
    Introduzca la coordenada x2 (x inferior derecha):
    5
    Introduzca la coordenada y2 (y inferior derecha):
    6
    ERROR, existe solapamiento entre las provincias
    ¿Desea introducir un nuevo rectángulo? (S):

    EJEMPLO 2:
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    1
    Introduzca la coordenada y1 (y superior izquierda):
    2
    Introduzca la coordenada x2 (x inferior derecha):
    3
    Introduzca la coordenada y2 (y inferior derecha):
    4
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Verde
    ¿Desea introducir un nuevo color? (S):

    ........................
    vvv.....................
    vvv.....................
    vvv.....................
    ........................
    ........................
    ........................
    ........................
    ........................
    ........................
    ........................
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    4
    Introduzca la coordenada y1 (y superior izquierda):
    2
    Introduzca la coordenada x2 (x inferior derecha):
    5
    Introduzca la coordenada y2 (y inferior derecha):
    4
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Rojo
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Verde
    ¿Desea introducir un nuevo color? (S):

    ........................
    vvvrr...................
    vvvrr...................
    vvvrr...................
    ........................
    ........................
    ........................
    ........................
    ........................
    ........................
    ........................
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    3
    Introduzca la coordenada y1 (y superior izquierda):
    5
    Introduzca la coordenada x2 (x inferior derecha):
    5
    Introduzca la coordenada y2 (y inferior derecha):
    7
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Rojo
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Verde
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Azul
    ¿Desea introducir un nuevo color? (S):

    ........................
    rrraa...................
    rrraa...................
    rrraa...................
    ..vvv...................
    ..vvv...................
    ..vvv...................
    ........................
    ........................
    ........................
    ........................
    ¿Desea introducir un nuevo rectángulo? (S):

--}