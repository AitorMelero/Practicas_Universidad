{--
	Fichero: B.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 29/10/2020
	Funcionalidad: Implementacion del apartado b de la P2a.
--}

{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoB.B where

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

-- Asignacion de provincias
hu = Provincia (4, 6) (5, 8)
ca = Provincia (6, 8) (8, 9)
se = Provincia (6, 5) (10, 7)
ma = Provincia (9, 8) (16, 9)
co = Provincia (11, 6) (16, 7)
ja = Provincia (13, 3) (16, 5)
gr = Provincia (17, 4) (19, 8)
al = Provincia (20, 6) (22, 8)

-- Funcion que indica si dos provincias son vecinas
sonVecinos :: Provincia -> Provincia -> Bool
sonVecinos (Provincia (x11, y11) (x12, y12)) (Provincia (x21, y21) (x22, y22))
    | x11 == x21 && x12 == x22 && y11 == y21 && y12 == y22 = False
    | (x11 == (x22 + 1) || x12 == (x21 - 1) || x11 == (x22 - 1) || x12 == (x21 + 1)) && not (null ([y11 .. y12] `intersect` [y21 .. y22])) = 
        True
    | (y11 == (y22 + 1) || y12 == (y21 - 1) || y11 == (y22 - 1) || y12 == (y21 + 1)) && not (null ([x11 .. x12] `intersect` [x21 .. x22])) =
        True
    | otherwise = False

-- Tipo Frontera
type Frontera = Provincia -> [Provincia]

-- Funcion que devuelve una lista de fronteras para una lista de provincias
fronteras :: [Provincia] -> Provincia -> [Provincia]
fronteras lista provincia = filter (sonVecinos provincia) lista
 
-- Funcion que devuelve la frontera para cada provincia de Andalucia
frAndalucia :: Frontera
frAndalucia x
    | x == al = [gr]
    | x == ca = [hu,se,ma]
    | x == co = [se,ma,ja,gr]
    | x == gr = [ma,co,ja,al]
    | x == ja = [co,gr]
    | x == hu = [ca,se]
    | x == ma = [ca,se,co,gr]
    | x == se = [hu,ca,ma,co]
    | otherwise = []

-- Tipo Mapa
data Mapa = Atlas [Provincia] Frontera
-- Funcion que devuelve el mapa de Andalucia
andalucia :: Mapa
andalucia = Atlas [al, ca, co, gr, ja, hu, ma, se] frAndalucia

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
-- Solucion con 3 colores para Andalucia
sol1 = solucionColorear (andalucia, [Rojo .. Azul]) -- encuentra una solucion
-- Solucion con 2 colores para Andalucia
sol2 = solucionColorear (andalucia, [Rojo,Verde]) -- sin solución

-- Main para el apartado B 
mainApartadoB :: IO ()
mainApartadoB = do dibujarMosaico (incluirProvincias mosaicoInicial sol1 )

{--
    Resultado esperado:

    ........................
    ........................
    ............vvvv........
    ............vvvvrrr.....
    .....rrrrr..vvvvrrr.....
    ...vvrrrrraaaaaarrrvvv..
    ...vvrrrrraaaaaarrrvvv..
    ...vvaaavvvvvvvvrrrvvv..
    .....aaavvvvvvvv........
    ........................
    ........................
--}