{--
	Fichero: C.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 29/10/2020
	Funcionalidad: Implementacion del apartado c de la P2a.
--}

{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoC.C where

import Data.List

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
data Color = Rojo | Verde | Azul deriving (Show,Enum,Eq)

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
solucionColorear = head . coloreados

-- Main para el apartado C
mainApartadoC :: IO ()
mainApartadoC = do print "######################### CASO CORRECTO ################################"
                   let p1 = crearProvincia (4, 6) (5, 8)
                       p2 = crearProvincia (6, 8) (8, 9)
                       p3 = crearProvincia (6, 5) (10, 7)
                       p4 = crearProvincia (9, 8) (16, 9)
                       p5 = crearProvincia (11, 6) (16, 7)
                       p6 = crearProvincia (13, 3) (16, 5)
                       p7 = crearProvincia (17, 4) (19, 8)
                       p8 = crearProvincia (20, 6) (22, 8)
                       lista1 = [p1,p2,p3,p4,p5,p6,p7,p8]
                       listaComprueba = compruebaProvincias lista1
                   if not listaComprueba then
                       print msg
                   else
                       do let lista2 = convierteListaProvincias lista1
                              listaComprueba2 = compruebaFronteras lista2
                          if listaComprueba2 then
                              do let mapa1 = mapaAux (sacarProvincia lista2)
                                     sol1 = solucionColorear (mapa1, [Rojo .. Azul])
                                 dibujarMosaico (incluirProvincias mosaicoInicial sol1)
                          else
                              print msg'
                   do print "######################### CASO INCORRECTO: COORDENADA INCORRECTA ################################" 
                      let p1' = crearProvincia (4, 6) (5, 8)
                          p2' = crearProvincia (46, 8) (8, 9)
                          lista1' = [p1',p2']
                          listaComprueba' = compruebaProvincias lista1'
                      if not listaComprueba' then
                          print msg
                      else
                          do let lista2' = convierteListaProvincias lista1'
                                 listaComprueba2' = compruebaFronteras lista2'
                             if listaComprueba2' then
                                  do let mapa1' = mapaAux (sacarProvincia lista2')
                                         sol1' = solucionColorear (mapa1', [Rojo .. Azul])
                                     dibujarMosaico (incluirProvincias mosaicoInicial sol1')
                             else
                                  print msg'
                   do print "######################### CASO INCORRECTO: SUPERPOSICION DE PROVINCIAS ################################" 
                      let p1'' = crearProvincia (4, 6) (5, 8)
                          p2'' = crearProvincia (4, 6) (8, 9)
                          lista1'' = [p1'',p2'']
                          listaComprueba'' = compruebaProvincias lista1''
                      if not listaComprueba'' then
                          print msg
                      else
                          do let lista2'' = convierteListaProvincias lista1''
                                 listaComprueba2'' = compruebaFronteras lista2''
                             if listaComprueba2'' then
                                  do let mapa1'' = mapaAux (sacarProvincia lista2'')
                                         sol1'' = solucionColorear (mapa1'', [Rojo .. Azul])
                                     dibujarMosaico (incluirProvincias mosaicoInicial sol1'')
                             else
                                  print msg'
                   

{--
    Resultado esperado:

    "######################### CASO CORRECTO ################################"
    ........................
    ........................
    ............rrrr........
    ............rrrrvvv.....
    .....vvvvv..rrrrvvv.....
    ...rrvvvvvaaaaaavvvrrr..
    ...rrvvvvvaaaaaavvvrrr..
    ...rraaarrrrrrrrvvvrrr..
    .....aaarrrrrrrr........
    ........................
    ........................
    "######################### CASO INCORRECTO: COORDENADA INCORRECTA ################################"
    "ERROR, valores incorrectos para coordenadas"
    "######################### CASO INCORRECTO: SUPERPOSICION DE PROVINCIAS ################################"
    "ERROR, existe solapamiento entre las provincias"
--}