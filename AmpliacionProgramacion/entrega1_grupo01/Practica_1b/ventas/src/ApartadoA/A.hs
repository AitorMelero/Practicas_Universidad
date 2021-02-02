{--
	Fichero: A.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 17/10/2020
	Funcionalidad: Implementacion del apartado 'a' de P1b.
--}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoA.A where

-- Nota: para operadores tenemos facturas ($) articulos (#) ventas (!) cantidad (%)

-- Definicion de tipos
type Cantidad = Int
data Articulo = Articulo {codigo::Int, nombre::[Char], precio::Float}
type Venta = (Articulo, Cantidad)
type Factura = [Venta]

-- Funcion que devuelve el precio de una venta
precioVenta :: Venta -> Float
precioVenta (a,c) = precio a * fromIntegral c

-- Funcion que devuelve el precio de una factura
precioFactura :: Factura -> Float
precioFactura [] = 0
precioFactura (f:fs) = precioVenta f + precioFactura fs

-- Operador para la fusion de facturas
($+$) :: Factura -> Factura -> Factura
f1 $+$ f2 = f1 ++ f2

-- Saca el articulo de una venta, necesaria por no tener deriving
sacarArticuloVenta :: Venta -> [Char]
sacarArticuloVenta (a,_) = nombre a

-- Operador para sacar el precio total de un articulo
($#) :: Factura -> Articulo -> Float
[] $# _ = 0
(v:vs) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v + (vs $# a)
            | otherwise = vs $# a

-- Funcion que devuelve el articulo en formato de cadena
articuloACadena :: Articulo -> String
articuloACadena a = "Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)

-- Funcion que devuelve una venta en formato de cadena
ventaACadena :: Venta -> String
ventaACadena (a, c) = articuloACadena a ++ " comprado " ++ show c ++ "veces"

-- Funcion que devuelve una factura en formato de cadena
facturaACadena :: Factura -> String
facturaACadena [] = "Factura vacia"
facturaACadena [v] = ventaACadena v
facturaACadena (v:vs) = ventaACadena v ++ " || " ++ facturaACadena vs

-- Operador para sacar las ventas de un articulo en una factura
(!#) :: Factura -> Articulo -> Int
[] !# a = 0
(v:vs) !# a
    | sacarArticuloVenta v == nombre a = cantidadVenta v + (vs !# a)
    | otherwise = vs !# a
    where
        cantidadVenta (a, c) = c

-- Operador para sacar las ventas de los articulos de una factura
(!##) :: Factura -> [Articulo] -> Int
[] !## a = 0
f !## [a] = f !# a 
f !## (a:as) = (f !# a) + (f !## as)

-- Operador que elimina articulos de una factura en funcion del articulo
(-##) :: Factura -> Articulo -> Factura
[] -## a = []
(v:vs) -## a
    | sacarArticuloVenta v == nombre a = vs -## a
    | otherwise = v : vs -## a

-- Operador que elimina articulo de una factura en relacion a la cantidad
(-%) :: Factura -> Int -> Factura
[] -% n = []
(v:vs) -% n
    | (v:vs) !# a < n = ((v:vs) -## a) -% n
    | otherwise = v : vs -% n
    where a = sacarArticulo v
          sacarArticulo (a,x) = a

-- main del apartado A
mainApartadoA :: IO ()
mainApartadoA = do let a1 = Articulo 1 "Tornillo" 3.4
                   let a2 = Articulo 2 "Martillo" 13.45
                   let a3 = Articulo 3 "Taladradora" 23.45
                   let v1 = (a1, 2)
                   let v2 = (a2, 3)
                   let v3 = (a3, 3)
                   let f1 = [v1, v2, v3]
                   print $ precioVenta v1
                   print $ precioVenta v2
                   print $ precioVenta v3
                   print $ precioFactura f1
                   print $ f1 $# a1
                   print $ articuloACadena a1
                   print $ articuloACadena a2
                   print $ articuloACadena a3
                   print $ ventaACadena v1
                   print $ ventaACadena v2
                   print $ ventaACadena v3
                   print $ facturaACadena f1
                   print $ f1 !# a1
                   print $ f1 !## [a1, a2]
                   print $ facturaACadena (f1 -% 3)

{--
    Resultado esperado:

        6.8
        40.35
        70.350006
        117.50001
        6.8
        "Articulo Tornillo con codigo 1 y precio: 3.4"
        "Articulo Martillo con codigo 2 y precio: 13.45"
        "Articulo Taladradora con codigo 3 y precio: 23.45"
        "Articulo Tornillo con codigo 1 y precio: 3.4 comprado 2veces"
        "Articulo Martillo con codigo 2 y precio: 13.45 comprado 3veces"
        "Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3veces"
        "Articulo Tornillo con codigo 1 y precio: 3.4 comprado 2veces || Articulo Martillo con codigo 2 y precio: 13.45 comprado 3veces || Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3veces"
        2
        5
        "Articulo Martillo con codigo 2 y precio: 13.45 comprado 3veces || Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3veces"
--}