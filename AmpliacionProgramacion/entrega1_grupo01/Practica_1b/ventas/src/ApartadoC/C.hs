{--
	Fichero: C.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 19/10/2020
	Funcionalidad: Implementacion del apartado 'c' de P1b.
--}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoC.C where

import Data.List

type Cantidad = Int
          
data Articulo = Articulo {codigo::Int, nombre::String, precio::Float}
data Venta = VentaUnitaria Articulo | VentaMultiple Articulo Cantidad | Articulo :+ Cantidad
data Factura = FacturaVacia | FacturaUnitaria Venta | FacturaMultiple [Venta]

instance Show Articulo where 
    show a = "Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)

instance Show Venta where 
    show (VentaUnitaria a) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)
    show (VentaMultiple a c) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a) ++ " comprado " ++ show c ++ " veces"
    show (a :+ 1) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)
    show (a :+ c) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a) ++ " comprado " ++ show c ++ " veces"

instance Show Factura where 
    show FacturaVacia = "Factura Vacia"
    show (FacturaUnitaria v) = "Factura con una sola venta: " ++ show v
    show (FacturaMultiple [v]) = show v
    show (FacturaMultiple (v:vs)) = show v ++ " || " ++ show (FacturaMultiple vs)


precioVenta :: Venta -> Float
precioVenta (VentaUnitaria a) = precio a
precioVenta (VentaMultiple a c) = precio a * fromIntegral c
precioVenta (a :+ 1) = precio a
precioVenta (a :+ c) = precio a * fromIntegral c

precioFactura :: Factura -> Float
precioFactura FacturaVacia = 0
precioFactura (FacturaUnitaria v) = precioVenta v
precioFactura (FacturaMultiple [v]) = precioVenta v
--CAMBIO 1
precioFactura (FacturaMultiple (v:vs)) = foldr (+) 0 [precioVenta p | p <- v:vs]

-- fusionFacturas ($+$)
($+$) :: Factura -> Factura -> Factura
(FacturaUnitaria v1) $+$ FacturaVacia = FacturaUnitaria v1
FacturaVacia $+$ (FacturaUnitaria v2) = FacturaUnitaria v2
(FacturaMultiple v1) $+$ FacturaVacia = FacturaMultiple v1
FacturaVacia $+$ (FacturaMultiple v2) = FacturaMultiple v2
(FacturaUnitaria v1) $+$ (FacturaUnitaria v2) = FacturaMultiple [v1, v2]
(FacturaUnitaria v1) $+$ (FacturaMultiple v2) = FacturaMultiple (v1:v2)
(FacturaMultiple v1) $+$ (FacturaUnitaria v2) = FacturaMultiple (v2:v1)
(FacturaMultiple v1) $+$ (FacturaMultiple v2) = FacturaMultiple (v1 ++ v2)

-- Funciones get, necesaria por no tener deriving
sacarArticuloVenta :: Venta -> String
sacarArticuloVenta (VentaUnitaria a) = nombre a
sacarArticuloVenta (VentaMultiple a _) = nombre a
sacarArticuloVenta (a :+ _) = nombre a

articulo :: Venta -> Articulo
articulo (VentaUnitaria a) = a
articulo (VentaMultiple a _) = a
articulo (a :+ _) = a

cantidad :: Venta -> Cantidad
cantidad (VentaUnitaria _) = 1
cantidad (VentaMultiple _ c) = c
cantidad (_ :+ c) = c

-- precioTotalArticulo ($#)
($#) :: Factura -> Articulo -> Float
(FacturaUnitaria v) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple [v]) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
-- CAMBIO 2
(FacturaMultiple (v:vs)) $# a = foldr (+) 0 [precioVenta p | p <- v:vs, sacarArticuloVenta p == nombre a]

-- ventasArticulo (!#)
(!#) :: Factura -> Articulo -> Int
FacturaVacia !# _ = 0
(FacturaUnitaria v) !# a
            | sacarArticuloVenta v == nombre a = 1
            | otherwise = 0
(FacturaMultiple [v]) !# a
            | sacarArticuloVenta v == nombre a = cantidadVenta v
            | otherwise = 0
            where
                  cantidadVenta (VentaUnitaria _) = 1
                  cantidadVenta (VentaMultiple _ c) = c
                  cantidadVenta (_ :+ c) = c
-- CAMBIO 3
(FacturaMultiple (v:vs)) !# a = cantidadVenta [v1 | v1 <- v:vs, sacarArticuloVenta v1 == nombre a]
                                where
                                      cantidadVenta [VentaUnitaria _] = 1
                                      cantidadVenta [VentaMultiple _ c] = c
                                      cantidadVenta [_ :+ c] = c

-- ventasArticulos (!##)
(!##) :: Factura -> [Articulo] -> Int
f !## [a] = f !# a 
-- CAMBIO 4
f !## (a:as) = foldr (+) 0 [f !# a' | a' <- a:as]

-- eliminacionArticulos (-##)
(-##) :: Factura -> Articulo -> Factura
FacturaVacia -## _ = FacturaVacia
(FacturaUnitaria v) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple [v]) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
-- CAMBIO 5
(FacturaMultiple (v:vs)) -## a = f'' f'
                        where f' = [ f | f <- v:vs, sacarArticuloVenta f /= nombre a]
                              f'' [] = FacturaVacia
                              f'' [x] = FacturaUnitaria x
                              f'' (x:xs) = FacturaMultiple (x:xs)

-- eliminacionCantida d (-%)
(-%) :: Factura -> Int -> Factura
FacturaVacia -% _ = FacturaVacia
(FacturaUnitaria v) -% n
    | cantidadVenta v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
    where
          cantidadVenta (VentaUnitaria _) = 1
          cantidadVenta (VentaMultiple _ c) = c
          cantidadVenta (_ :+ c) = c
(FacturaMultiple [v]) -% n
    | cantidadVenta v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
    where
          cantidadVenta (VentaUnitaria _) = 1
          cantidadVenta (VentaMultiple _ c) = c
          cantidadVenta (_ :+ c) = c
-- CAMBIO 6
(FacturaMultiple (v:vs)) -% n = f'' f'
                        where f' = [ f | f <- v:vs, cantidadVenta f >= n]
                              cantidadVenta (VentaUnitaria _) = 1
                              cantidadVenta (VentaMultiple _ c) = c
                              cantidadVenta (_ :+ c) = c 
                              f'' [] = FacturaVacia
                              f'' [x] = FacturaUnitaria x
                              f'' (x:xs) = FacturaMultiple (x:xs)

-- Funcion extra del apartado C
eliminacionArticulosRepetidos :: Factura -> Factura
eliminacionArticulosRepetidos (FacturaMultiple (v:vs)) = FacturaMultiple (map sumarCantidad (agrupar (v:vs)))
                        where sumarCantidad :: [Venta] -> Venta
                              sumarCantidad (v':vs') =  VentaMultiple (articulo v') (sum [cantidad x | x <- v':vs'])
                              agrupar :: [Venta] -> [[Venta]]
                              agrupar (z:zs) = groupBy esCodigoIgual (ordenar (z:zs))
                              esCodigoIgual :: Venta -> Venta ->Bool
                              esCodigoIgual v1 v2 = codigo (articulo v1) == codigo (articulo v2)
                              ordenar :: [Venta] -> [Venta]
                              ordenar [] = []
                              ordenar (x:xs) = ordenar [ y | y <- xs, codigo (articulo (head xs)) <= codigo (articulo x)] ++ [x] ++ ordenar [ z | z <- xs, codigo (articulo (head xs)) > codigo (articulo x)]

-- Main
mainApartadoC :: IO ()
mainApartadoC = do let a1 = Articulo 1 "Tornillo" 3.4
                   let a2 = Articulo 2 "Martillo" 13.45
                   let a3 = Articulo 3 "Taladradora" 23.45
                   let a4 = Articulo 2 "Martillo" 13.45
                   let v1 = VentaUnitaria a1
                   let v2 = a2 :+ 2
                   let v3 = VentaMultiple a3 3
                   let v4 = a4 :+ 3
                   let f1 = FacturaMultiple [v1, v2, v3]
                   let f2 = FacturaUnitaria v1
                   let f3 = FacturaMultiple [v1, v2, v3, v4]
                   print $ precioVenta v1
                   print $ precioVenta v2
                   print $ precioVenta v3
                   print $ precioFactura f1
                   print $ precioFactura f2
                   print $ precioFactura (f1 $+$ f2)
                   print $ f1 $# a1
                   print $ show a1
                   print $ show a2
                   print $ show a3
                   print $ show v1
                   print $ show v2
                   print $ show v3
                   print $ show f1
                   print $ f1 !# a1
                   print $ f1 !## [a1, a2]
                   print $ f1 -## a1
                   print $ f1 -% 3
                   print $ show f3
                   print $ eliminacionArticulosRepetidos f3
                   

{--
    Resultado esperado:

        3.4
        26.9
        70.350006
        100.65001
        3.4
        104.05001
        3.4
        "Articulo Tornillo con codigo 1 y precio: 3.4"
        "Articulo Martillo con codigo 2 y precio: 13.45"
        "Articulo Taladradora con codigo 3 y precio: 23.45"
        "Venta => Articulo Tornillo con codigo 1 y precio: 3.4"
        "Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces"
        "Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces"
        "Venta => Articulo Tornillo con codigo 1 y precio: 3.4 || Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces || Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces"
        1
        3
        Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces || Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces
        Factura con una sola venta: Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces
        "Venta => Articulo Tornillo con codigo 1 y precio: 3.4 || Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces || Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces || Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 3 veces"
        Venta => Articulo Tornillo con codigo 1 y precio: 3.4 comprado 1 veces || Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 5 veces || Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces
--}