{--
	Fichero: E.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 30/10/2020
	Funcionalidad: Implementacion del apartado e de la P2a.
--}

{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoE.E where

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

-- Tipo Region
type Region = [Provincia]

-- Tipo Frontera
type Frontera = Provincia -> [Provincia]

-- Tipo Frontera' para las fronteras de regiones
type Frontera' = Region -> [Region]

-- Mensaje de error por coordenadas en variable global y solapamiento
msg :: String
msg = "ERROR, valores incorrectos para coordenadas."
msg' :: String
msg' = "ERROR, existe solapamiento entre las provincias."
-- Mensaje de error para las regiones
msg'' = "ERROR, este conjunto de provincias no forman parte de una misma region."
msg''' = "ERROR, existe solapamiento entre las regiones."

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

-- Funcion que comprueba para una lista de provincias si no existe solapamiento entre ninguna
noSolapamiento :: [Provincia] -> Bool
noSolapamiento (p:ps) = compruebaFronteras (Right (p:ps))

-- Funcion que comprueba para una lista de provincias, si todas tienen al menos un vecino
todosVecinos :: [Provincia] -> Bool
todosVecinos (p:ps) = let listaFronteras = [fronteras' f (p:ps) | f <- p:ps] in
                      Right [] `notElem` listaFronteras && Left msg' `notElem` listaFronteras

-- Funcion que comprueba si una lista de provincias cumple los requisitos para ser una region
comprobarRegion :: [Provincia] -> Bool
comprobarRegion (p:ps) = noSolapamiento (p:ps) && todosVecinos (p:ps)

-- Funcion que crea una Region
crearRegion :: [Provincia] -> Either String Region
crearRegion [] = Right []
crearRegion [p] = Right [p]
crearRegion (p:ps) = if comprobarRegion (p:ps) then Right (p:ps) else Left msg''

-- Funcion que convierte un Right Provincia en un Provincia
convierteProvincia :: Either String Provincia -> Provincia
convierteProvincia (Right (Provincia (x1,y1) (x2,y2))) = Provincia (x1,y1) (x2,y2)

-- Funcion que convierte un Right Region en un Region
convierteRegion :: Either String Region -> Region
convierteRegion (Right r) = r

-- Funcion que genera un rectangulo como area
generaRectangulo :: (Integer, Integer) -> (Integer, Integer) -> Integer -> [Integer]
generaRectangulo (x1, y1) (x2, y2) x
                                        | y1 > y2 = []
                                        | x1 == x2 = (x1+(anchura'*(y1-1))) : generaRectangulo (x, y1+1) (x2, y2) x
                                        | otherwise = (x1+(anchura'*(y1-1))) : generaRectangulo (x1+1, y1) (x2, y2) x

-- Funcion que genera el borde de un rectangulo
generaRectangulo' :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Integer -> [Integer]
generaRectangulo' (x1, y1) (x2, y2) x y
                                        | y1 > y2 = []
                                        | x1 == x2 = (x1+(anchura'*(y1-1))) : generaRectangulo' (x, y1+1) (x2, y2) x y
                                        | y1 == y2 = (x1+(anchura'*(y1-1))) : generaRectangulo' (x1+1, y1) (x2, y2) x y
                                        | x1 == x = (x1+(anchura'*(y1-1))) : generaRectangulo' (x1+1, y1) (x2, y2) x y
                                        | y1 == y = (x1+(anchura'*(y1-1))) : generaRectangulo' (x1+1, y1) (x2, y2) x y
                                        | otherwise = generaRectangulo' (x1+1, y1) (x2, y2) x y

-- Funcion que genera el area de una region
generaArea :: Region -> [Integer]
generaArea [] = []
generaArea [Provincia (x1,y1) (x2,y2)] = generaRectangulo (x1,y1) (x2,y2) x1
generaArea (p:ps) = let listaRectangulos = [generaArea [p'] | p' <- p:ps] in
                    foldl (++) (head listaRectangulos) (tail listaRectangulos)

-- Funcion que genera el borde de una region
generaBorde :: Region -> [Integer]
generaBorde [] = []
generaBorde [Provincia (x1,y1) (x2,y2)] = generaRectangulo' (x1,y1) (x2,y2) x1 y1
generaBorde (p:ps) = let listaBorde = [generaBorde [p'] | p' <- p:ps] in
                     foldl (++) (head listaBorde) (tail listaBorde)

-- Funcion que genera el borde auxiliar de una region para comprobar las regiones vecinas
generaBordeAux :: Region -> [Integer]
generaBordeAux [] = []
generaBordeAux r = let borde = generaBorde r
                       bordeArriba = [ba - anchura' | ba <- borde]
                       bordeDerecha = [if anchura' `mod` bd == 0 then bd else bd + 1  | bd <- borde]
                       bordeAbajo = [bb + anchura' | bb <- borde]
                       bordeIzquierda= [if anchura' `mod` bd == 1 then bd else bd - 1  | bd <- borde]
                       bordeAux = bordeArriba ++ bordeDerecha ++ bordeAbajo ++ bordeIzquierda in
                   bordeAux

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

-- Funcion que indica si dos regiones son vecinas
sonVecinos'' :: Region -> Region -> Either String Bool
sonVecinos'' r1 r2 
    | r1 == r2 = Left msg'''
    | not (null (generaArea r1 `intersect` generaArea r2)) = Left msg'''
    | not (null (generaBorde r1 `intersect` generaBordeAux r2)) =
        return True
    | otherwise = return False

-- Funcion que saca provincias de una lista de Either de provincias
sacarProvincia :: Either String [Provincia] -> [Provincia]
sacarProvincia (Left _) = []
sacarProvincia (Right []) = []
sacarProvincia (Right [x]) = [x]
sacarProvincia (Right (x:xs)) = x:xs

-- Funcion que saca regiones de una lista de Either de regiones
sacarRegiones :: Either String [Region] -> [Region]
sacarRegiones (Left _) = []
sacarRegiones (Right []) = []
sacarRegiones (Right [x]) = [x]
sacarRegiones (Right (x:xs)) = x:xs

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

-- Funcion que devuelve una lista de fronteras para una lista de regiones
fronteras'' :: Region -> [Region] -> Either String [Region]
fronteras'' _ [] = Right []
fronteras'' region [x] = do let vfront = sonVecinos'' region x
                            if region == x then
                                return []
                            else if vfront == Left msg''' then
                                Left msg'''
                            else if vfront == Right True then
                                return [x]
                            else
                                return []
fronteras'' region (x:xs) = if region == x then
                                    fronteras'' region xs else
                              do let vfront = sonVecinos'' region x 
                                     vfront' = fronteras'' region xs
                                 if (vfront == Left msg''') || (vfront' == Left msg''') then
                                     Left msg'''
                                 else if vfront == Right True then
                                     return (x : sacarRegiones vfront')
                                 else
                                     return (sacarRegiones vfront')

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

-- Funcion que ayuda a comprobar las regiones con >>=
compruebaRegion :: Region -> Either String Region
compruebaRegion = Right

-- Funcion que comprueba que no hay errores y se puede sacar el mapa de regiones
compruebaRegiones :: [Either String Region] -> Bool
compruebaRegiones [] = True
compruebaRegiones [x] = do let comp = x >>= compruebaRegion
                           comp /= Left msg

compruebaRegiones (x:xs) = do let res = Left msg `notElem` (x:xs)
                              res

-- Funcion que comprueba las fronteras
compruebaFronteras :: Either String [Provincia] -> Bool
compruebaFronteras (Left _) = False
compruebaFronteras (Right []) = True
compruebaFronteras (Right [x]) = True
compruebaFronteras (Right (x:xs)) = Left msg' `notElem` [fronteras' x' (x:xs) | x' <- x:xs]

-- Funcion que comprueba las fronteras para las regiones
compruebaFronteras' :: Either String [Region] -> Bool
compruebaFronteras' (Left _) = False
compruebaFronteras' (Right []) = True
compruebaFronteras' (Right [x]) = True
compruebaFronteras' (Right (x:xs)) = Left msg''' `notElem` [fronteras'' x' (x:xs) | x' <- x:xs]

-- Funcion que convierte una lista de Either a una lista Either de Provincias
convierteListaProvincias :: [Either String Provincia] -> Either String [Provincia]
convierteListaProvincias [] = Right []
convierteListaProvincias [x] = Right [convierteProvincia x]
convierteListaProvincias (x:xs) = Right [convierteProvincia l | l <- x:xs]

-- Funcion que convierte una lista de Either a una lista Either de Region
convierteListaRegiones :: [Either String Region] -> Either String [Region]
convierteListaRegiones [] = Right []
convierteListaRegiones [x] = Right [convierteRegion x]
convierteListaRegiones (x:xs) = Right [convierteRegion l | l <- x:xs]



-- Funcion que saca las fronteras de una provincia
fr :: [Provincia] -> Frontera
fr (x:xs) p = sacarProvincia (fronteras' p (x:xs))

-- Funcion que saca las fronteras de una region
fr' :: [Region] -> Frontera'
fr' (r:rs) r' = sacarRegiones (fronteras'' r' (r:rs))

-- Tipo Mapa
data Mapa = Atlas [Provincia] Frontera

-- Tipo Mapa' para regiones
data Mapa' = Atlas' [Region] Frontera'

-- Mapa con provincias
mapaAux :: [Provincia] -> Mapa
mapaAux (x:xs) = Atlas (x:xs) (fr (x:xs))

-- Mapa con regiones
mapaAux' :: [Region] -> Mapa'
mapaAux' (r:rs) = Atlas' (r:rs) (fr' (r:rs))
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- Colores de provincias vecinas para un coloreado
coloresFrontera ::
    Provincia->[(Provincia,Color)]->Frontera-> [Color]
coloresFrontera provincia coloreado frontera
    = [col | (prov,col)<- coloreado, prov `elem` frontera provincia]

-- Colores de provincias vecinas para un coloreado de regiones
coloresFrontera' :: Region->[(Region,Color)]->Frontera'-> [Color]
coloresFrontera' region coloreado frontera
    = [col | (reg,col)<- coloreado, reg `elem` frontera region]

-- Posibles coloreados para un mapa y una lista de colores
coloreados :: (Mapa,[Color]) -> [[(Provincia,Color)]]
coloreados (Atlas [] _, _) = [[]]
coloreados (Atlas (prov:provs) frontera, colores)
    = [(prov,color):coloreado' |
        coloreado' <- coloreados
            (Atlas provs frontera, colores)
        , color <- colores \\
            coloresFrontera prov coloreado' frontera]

-- Posibles coloreados para un mapa de regiones y una lista de colores
coloreados' :: (Mapa',[Color]) -> [[(Region,Color)]]
coloreados' (Atlas' [] _, _) = [[]]
coloreados' (Atlas' (prov:provs) frontera, colores)
    = [(prov,color):coloreado' |
        coloreado' <- coloreados'
            (Atlas' provs frontera, colores)
        , color <- colores \\
            coloresFrontera' prov coloreado' frontera]

-- Funcion que incluye las provincias en el mosaico
incluirProvincias :: Mosaico -> [(Provincia,Color)] -> Mosaico
incluirProvincias = foldl incluirProvincia  -- incluirProvincia es la funcion de pliegue

-- Funcion que incluye las regiones en el mosaico
incluirRegiones :: Mosaico -> [(Region,Color)] -> Mosaico
incluirRegiones = foldl incluirRegion  -- incluirRegion es la funcion de pliegue

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

-- Superpone el cuadrado en las posiciones del mosaico
incluirRegion :: Mosaico -> (Region,Color) -> Mosaico
incluirRegion mosaico (region,color)  = map fila mapa_altura 
        where mapa_altura :: [Integer] 
              mapa_altura = [1..altura']
              fila n = map (letra n) mapa_anchura 
              mapa_anchura :: [Integer]
              mapa_anchura = [1..anchura']
              (letra n) m | dentroRegion n m region = getLetra color
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

-- Funcion que comprueba si una coordenada esta dentro de una region o no
dentroRegion :: Integer -> Integer -> Region -> Bool
dentroRegion n m region = coordenada `elem` generaArea region
                        where coordenada = m + (n-1)*anchura'

-- Solucion al problema de los colores
solucionColorear:: (Mapa,[Color]) -> [(Provincia,Color)]
solucionColorear (m, color) = let sol' = coloreados (m, color) in
                                if sol' /= [] then head sol' else []

-- Solucion al problema de los colores para las regiones
solucionColorear':: (Mapa',[Color]) -> [(Region,Color)]
solucionColorear' (m, color) = let sol' = coloreados' (m, color) in
                                if sol' /= [] then head sol' else []

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
introducirRectangulo :: [Either String Provincia] -> IO [Provincia]
introducirRectangulo [] = do putStrLn "¿Desea introducir un nuevo rectángulo? (S):"
                             introducirRect <- getLine
                             if introducirRect == "S" then
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
                                                introducirRectangulo lista1
                                            else
                                                do putStrLn msg'
                                                   introducirRectangulo []
                             else
                                 do putStrLn "Debe introducir al menos un rectangulo."
                                    introducirRectangulo []
introducirRectangulo [listaRectangulos] = do putStrLn "¿Desea introducir un nuevo rectángulo? (S):"
                                             introducirRect <- getLine
                                             if introducirRect == "S" then
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
                                                               introducirRectangulo lista1
                                                            else
                                                                do putStrLn msg'
                                                                   introducirRectangulo [listaRectangulos]
                                             else
                                                 return (sacarProvincia (convierteListaProvincias [listaRectangulos]))
introducirRectangulo (r:rs) = do putStrLn "¿Desea introducir un nuevo rectángulo? (S):"
                                 introducirRect <- getLine
                                 if introducirRect == "S" then
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
                                                       introducirRectangulo lista1
                                                   else
                                                       do putStrLn msg'
                                                          introducirRectangulo (r:rs)
                                 else
                                     return (sacarProvincia (convierteListaProvincias (r:rs)))

-- Funcion a la que se llama desde el main para realizar la funcionalidad del apartado E
introducirRegion :: [Either String Region] -> IO ()
introducirRegion [] = do putStrLn "¿Desea introducir una nueva region? (S):"
                         introducirReg <- getLine
                         Control.Monad.when (introducirReg == "S") $
                            do rectangulos <- introducirRectangulo []
                               let region = crearRegion rectangulos
                               if region == Left msg'' then
                                   do putStrLn msg''
                                      introducirRegion []
                               else
                                   do let region' = convierteListaRegiones [region]
                                      if not (compruebaFronteras' region') then
                                          do putStrLn msg'''
                                             introducirRegion []
                                      else
                                          do colores <- introducirColores []
                                             let regiones = sacarRegiones region'
                                                 mapa1 = mapaAux' regiones
                                                 sol1 = solucionColorear' (mapa1, colores)
                                             if sol1 /= [] then
                                                 do dibujarMosaico (incluirRegiones mosaicoInicial sol1)
                                                    introducirRegion [region]
                                             else
                                                 do putStrLn "No hay solucion."
                                                    introducirRegion [region]
introducirRegion [listaRegiones] = do putStrLn "¿Desea introducir una nueva region? (S):"
                                      introducirReg <- getLine
                                      Control.Monad.when (introducirReg == "S") $
                                          do rectangulos <- introducirRectangulo []
                                             let region = crearRegion rectangulos
                                             if region == Left msg'' then
                                                 do putStrLn msg''
                                                    introducirRegion [listaRegiones]
                                             else
                                                 do let lista1 =  [listaRegiones, region]
                                                        region' = convierteListaRegiones lista1
                                                    if not (compruebaFronteras' region') then
                                                        do putStrLn msg'''
                                                           introducirRegion [listaRegiones]
                                                    else
                                                        do colores <- introducirColores []
                                                           let mapa1 = mapaAux' (sacarRegiones region')
                                                               sol1 = solucionColorear' (mapa1, colores)
                                                           if sol1 /= [] then
                                                               do dibujarMosaico (incluirRegiones mosaicoInicial sol1)
                                                                  introducirRegion lista1
                                                           else
                                                               do putStrLn "No hay solucion."
                                                                  introducirRegion lista1
introducirRegion (r:rs) = do putStrLn "¿Desea introducir una nueva region? (S):"
                             introducirReg <- getLine
                             Control.Monad.when (introducirReg == "S") $
                                   do rectangulos <- introducirRectangulo []
                                      let region = crearRegion rectangulos
                                      if region == Left msg'' then
                                           do putStrLn msg''
                                              introducirRegion (r:rs)
                                      else
                                           do let lista1 =  region:r:rs
                                                  region' = convierteListaRegiones lista1
                                              if not (compruebaFronteras' region') then
                                                  do putStrLn msg'''
                                                     introducirRegion (r:rs)
                                              else
                                                  do colores <- introducirColores []
                                                     let mapa1 = mapaAux' (sacarRegiones region')
                                                         sol1 = solucionColorear' (mapa1, colores)
                                                     if sol1 /= [] then
                                                         do dibujarMosaico (incluirRegiones mosaicoInicial sol1)
                                                            introducirRegion lista1
                                                     else
                                                         do putStrLn "No hay solucion."
                                                            introducirRegion lista1

-- Main para el apartado E 
mainApartadoE :: IO ()
mainApartadoE = introducirRegion []                   

{--
    Resultado esperado (ESTOS ES UN EJEMPLO ORIENTATIVO YA QUE ES EL USUARIO EL QUE INTRODUCE LOS DATOS POR TECLADO):

    ¿Desea introducir una nueva region? (S):
    S
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
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    4
    Introduzca la coordenada y1 (y superior izquierda):
    2
    Introduzca la coordenada x2 (x inferior derecha):
    5
    Introduzca la coordenada y2 (y inferior derecha):
    7
    ¿Desea introducir un nuevo rectángulo? (S):

    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Azul
    ¿Desea introducir un nuevo color? (S):

    ........................
    aaaaa...................
    aaaaa...................
    aaaaa...................
    ...aa...................
    ...aa...................
    ...aa...................
    ........................
    ........................
    ........................
    ........................
    ¿Desea introducir una nueva region? (S):
    S
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    6
    Introduzca la coordenada y1 (y superior izquierda):
    6
    Introduzca la coordenada x2 (x inferior derecha):
    8
    Introduzca la coordenada y2 (y inferior derecha):
    8
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    9
    Introduzca la coordenada y1 (y superior izquierda):
    5
    Introduzca la coordenada x2 (x inferior derecha):
    12
    Introduzca la coordenada y2 (y inferior derecha):
    7
    ¿Desea introducir un nuevo rectángulo? (S):

    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Rojo
    ¿Desea introducir un nuevo color? (S):

    No hay solucion.
    ¿Desea introducir una nueva region? (S):
    S
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    7
    Introduzca la coordenada y1 (y superior izquierda):
    1
    Introduzca la coordenada x2 (x inferior derecha):
    7
    Introduzca la coordenada y2 (y inferior derecha):
    1
    ¿Desea introducir un nuevo rectángulo? (S):

    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Verde
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Azul
    ¿Desea introducir un nuevo color? (S):

    ......v.................
    aaaaa...................
    aaaaa...................
    aaaaa...................
    ...aa...vvvv............
    ...aavvvvvvv............
    ...aavvvvvvv............
    .....vvv................
    ........................
    ........................
    ........................
    ¿Desea introducir una nueva region? (S):
    S
    ¿Desea introducir un nuevo rectángulo? (S):
    S
    Introduzca la coordenada x1 (x superior izquierda):
    8
    Introduzca la coordenada y1 (y superior izquierda):
    1
    Introduzca la coordenada x2 (x inferior derecha):
    12
    Introduzca la coordenada y2 (y inferior derecha):
    4
    ¿Desea introducir un nuevo rectángulo? (S):

    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Verde
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Azul
    ¿Desea introducir un nuevo color? (S):
    S
    Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
    Naranja
    ¿Desea introducir un nuevo color? (S):

    ......nvvvvv............
    vvvvv..vvvvv............
    vvvvv..vvvvv............
    vvvvv..vvvvv............
    ...vv...nnnn............
    ...vvnnnnnnn............
    ...vvnnnnnnn............
    .....nnn................
    ........................
    ........................
    ........................
    ¿Desea introducir una nueva region? (S):

--}