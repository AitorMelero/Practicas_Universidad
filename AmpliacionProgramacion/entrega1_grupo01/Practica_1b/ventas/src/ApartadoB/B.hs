{--
	Fichero: B.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 17/10/2020
	Funcionalidad: Implementacion del apartado 'b' de P1b.
--}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoB.B where

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
precioFactura (FacturaMultiple (v:vs)) = precioVenta v + precioFactura (FacturaMultiple vs)

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

-- Saca el articulo de una venta, necesaria por no tener deriving
sacarArticuloVenta :: Venta -> String
sacarArticuloVenta (VentaUnitaria a) = nombre a
sacarArticuloVenta (VentaMultiple a _) = nombre a
sacarArticuloVenta (a :+ _) = nombre a

-- precioTotalArticulo ($#)
($#) :: Factura -> Articulo -> Float
(FacturaUnitaria v) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple [v]) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple (v:vs)) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v + ((FacturaMultiple vs) $# a)
            | otherwise = (FacturaMultiple vs) $# a

-- ventasArticulo (!#)
(!#) :: Factura -> Articulo -> Int
FacturaVacia !# a = 0
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
(FacturaMultiple (v:vs)) !# a
          | sacarArticuloVenta v == nombre a = cantidadVenta v
          | otherwise = (FacturaMultiple vs) !# a
          where
                cantidadVenta (VentaUnitaria _) = 1
                cantidadVenta (VentaMultiple _ c) = c
                cantidadVenta (_ :+ c) = c

-- ventasArticulos (!##)
(!##) :: Factura -> [Articulo] -> Int
f !## [a] = f !# a 
f !## (a:as) = f !#  a + f !## as

-- eliminacionArticulos (-##)
(-##) :: Factura -> Articulo -> Factura
FacturaVacia -## a = FacturaVacia
(FacturaUnitaria v) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple [v]) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple (v:vs)) -## a
    | sacarArticuloVenta v == nombre a = (FacturaMultiple vs) -## a
    | otherwise = (FacturaUnitaria v) $+$ ((FacturaMultiple vs) -## a)

-- eliminacionCantida d (-%)
(-%) :: Factura -> Int -> Factura
FacturaVacia -% n = FacturaVacia
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
(FacturaMultiple (v:vs)) -% n
    | cantidadVenta v < n = (FacturaMultiple vs) -% n
    | otherwise = (FacturaUnitaria v) $+$ ((FacturaMultiple vs) -% n)
    where
          cantidadVenta (VentaUnitaria _) = 1
          cantidadVenta (VentaMultiple _ c) = c
          cantidadVenta (_ :+ c) = c

mainApartadoB :: IO ()
mainApartadoB = do let a1 = Articulo 1 "Tornillo" 3.4
                   let a2 = Articulo 2 "Martillo" 13.45
                   let a3 = Articulo 3 "Taladradora" 23.45
                   let v1 = VentaUnitaria a1
                   let v2 = a2 :+ 2
                   let v3 = VentaMultiple a3 3
                   let f1 = FacturaMultiple [v1, v2, v3]
                   let f2 = FacturaUnitaria v1
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
--}