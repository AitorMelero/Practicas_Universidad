{--
	Fichero: C.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 19/10/2020
	Funcionalidad: Implementacion del apartado 'c' de P1b.
--}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoH.H where

import Data.List

type Cantidad = Int

-- Tipos posibles de articulos
data TipoArticulo = ProductoBasico | ProductoImportante | ProductoLujo
instance Show TipoArticulo where
    show ProductoBasico = "Producto basico"
    show ProductoImportante = "Producto importante"
    show ProductoLujo = "Producto de lujo"

--Obtencion del IVA aplicado por el articulo
getIva :: TipoArticulo -> Float
getIva ProductoBasico = 0.04
getIva ProductoImportante = 0.1
getIva ProductoLujo = 0.21

-- Tipos de ventas
data TipoVenta = Normal | ADomicilio | Regalo | RegaloADomicilio
instance Show TipoVenta where
    show Normal = "Normal"
    show ADomicilio = "a domicilio"
    show Regalo = "Regalo"
    show RegaloADomicilio = "Regalo a domicilio"

-- Obtencion del extra de las nuevas ventas
getExtraVenta :: TipoVenta -> Float
getExtraVenta Normal = 0
getExtraVenta ADomicilio = 10
getExtraVenta Regalo = 5
getExtraVenta RegaloADomicilio = getExtraVenta ADomicilio + getExtraVenta Regalo

-- Promociones
data Promocion = Ninguna | DosPorTres | MitadPrecio deriving Eq

instance Show Promocion where
    show Ninguna = "Ninguna promocion"
    show DosPorTres = "Promocion 2x3"
    show MitadPrecio = "Promocion -50%"


data Articulo = Articulo {codigo::Int, nombre::String, precio::Float, tipo::TipoArticulo, promocion::Promocion}
data Venta = VentaUnitaria Articulo TipoVenta | VentaMultiple Articulo Cantidad TipoVenta | Articulo :+ Cantidad
data Factura = FacturaVacia | FacturaUnitaria Venta | FacturaMultiple [Venta]

instance Show Articulo where 
    show a = show (tipo a) ++ ": " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " precio: " ++ show (precio a) ++ " y con " ++ show (promocion a)


instance Show Venta where 
    show (VentaUnitaria a t) = "Venta " ++ show t ++ "=> " ++ show a
    show (VentaMultiple a c t) = "Venta " ++ show t ++ "=> " ++ show a ++ " comprado " ++ show c ++ " veces"
    show (a :+ 1) = "Venta => " ++ show a
    show (a :+ c) = "Venta => " ++ show a ++ " comprado " ++ show c ++ " veces"

instance Show Factura where 
    show FacturaVacia = "Factura Vacia"
    show (FacturaUnitaria v) = "Factura con una sola venta: " ++ show v
    show (FacturaMultiple [v]) = show v
    show (FacturaMultiple (v:vs)) = show v ++ " || " ++ show (FacturaMultiple vs)

precioArticulo :: Articulo -> Float
precioArticulo a = precio a * (1 + getIva (tipo a))

getTipoVenta :: Venta -> TipoVenta
getTipoVenta (VentaUnitaria _ tv) = tv
getTipoVenta (VentaMultiple _ _ tv) = tv
getTipoVenta (_ :+ 1) = Normal
getTipoVenta (_ :+ _) = Normal

getCantidadGratis :: Int -> Int
getCantidadGratis c = round (fromIntegral  c/3)

precioVenta :: Venta -> Float
precioVenta (VentaUnitaria a t) = precioArticulo a + getExtraVenta t
precioVenta (VentaMultiple a c t) 
    | promocion a == DosPorTres = (precioArticulo a * fromIntegral (c - getCantidadGratis c)) + getExtraVenta t
    | promocion a == MitadPrecio = ((precioArticulo a / 2) * fromIntegral c) + getExtraVenta t
    | otherwise = (precioArticulo a * fromIntegral c) + getExtraVenta t
precioVenta (a :+ 1) = precioArticulo a
precioVenta (a :+ c)
    | promocion a == DosPorTres = precioArticulo a * fromIntegral (c - getCantidadGratis c)
    | promocion a == MitadPrecio = (precioArticulo a / 2) * fromIntegral (c - getCantidadGratis c)
    | otherwise = precioArticulo a * fromIntegral c


precioFactura :: Factura -> Float
precioFactura FacturaVacia = 0
precioFactura (FacturaUnitaria v) = precioVenta v
precioFactura (FacturaMultiple [v]) = precioVenta v
--CAMBIO 1
precioFactura (FacturaMultiple (v:vs)) = foldr (+) 0 [precioVenta p | p <- v:vs]
-- TODO explicar que si no coges los productos juntos no se aplica la promoción y eres gilipollas

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
sacarArticuloVenta (VentaUnitaria a _) = nombre a
sacarArticuloVenta (VentaMultiple a _ _) = nombre a
sacarArticuloVenta (a :+ _) = nombre a

articulo :: Venta -> Articulo
articulo (VentaUnitaria a _) = a
articulo (VentaMultiple a _ _) = a
articulo (a :+ _) = a

cantidad :: Venta -> Cantidad
cantidad (VentaUnitaria _ _) = 1
cantidad (VentaMultiple _ c _) = c
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
                  cantidadVenta (VentaUnitaria _ _) = 1
                  cantidadVenta (VentaMultiple _ c _) = c
                  cantidadVenta (_ :+ c) = c
-- CAMBIO 3
(FacturaMultiple (v:vs)) !# a = cantidadVenta [v1 | v1 <- v:vs, sacarArticuloVenta v1 == nombre a]
                                where
                                      cantidadVenta [VentaUnitaria _ _] = 1
                                      cantidadVenta [VentaMultiple _ c _] = c
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
          cantidadVenta (VentaUnitaria _ _) = 1
          cantidadVenta (VentaMultiple _ c _) = c
          cantidadVenta (_ :+ c) = c
(FacturaMultiple [v]) -% n
    | cantidadVenta v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
    where
          cantidadVenta (VentaUnitaria _ _) = 1
          cantidadVenta (VentaMultiple _ c _) = c
          cantidadVenta (_ :+ c) = c
-- CAMBIO 6
(FacturaMultiple (v:vs)) -% n = f'' f'
                        where f' = [ f | f <- v:vs, cantidadVenta f >= n]
                              cantidadVenta (VentaUnitaria _ _) = 1
                              cantidadVenta (VentaMultiple _ c _) = c
                              cantidadVenta (_ :+ c) = c 
                              f'' [] = FacturaVacia
                              f'' [x] = FacturaUnitaria x
                              f'' (x:xs) = FacturaMultiple (x:xs)

-- Funcion extra del apartado C
eliminacionArticulosRepetidos :: Factura -> Factura
eliminacionArticulosRepetidos (FacturaMultiple (v:vs)) = FacturaMultiple (map sumarCantidad (agrupar (v:vs)))
                        where sumarCantidad :: [Venta] -> Venta
                              sumarCantidad (v':vs') =  VentaMultiple (articulo v') (sum [cantidad x | x <- v':vs']) (getTipoVenta v')
                              agrupar :: [Venta] -> [[Venta]]
                              agrupar (z:zs) = groupBy esCodigoIgual (ordenar (z:zs))
                              esCodigoIgual :: Venta -> Venta ->Bool
                              esCodigoIgual v1 v2 = codigo (articulo v1) == codigo (articulo v2)
                              ordenar :: [Venta] -> [Venta]
                              ordenar [] = []
                              ordenar (x:xs) = ordenar [ y | y <- xs, codigo (articulo (head xs)) <= codigo (articulo x)] ++ [x] ++ ordenar [ z | z <- xs, codigo (articulo (head xs)) > codigo (articulo x)]

-- Main
mainApartadoH :: IO ()
mainApartadoH = do let a1 = Articulo 1 "Tornillo" 3.4 ProductoBasico Ninguna
                   let a2 = Articulo 2 "Martillo" 13.45 ProductoImportante DosPorTres
                   let a3 = Articulo 3 "Taladradora" 23.45 ProductoLujo MitadPrecio
                   let a4 = Articulo 2 "Martillo" 13.45 ProductoLujo Ninguna
                   let v1 = VentaUnitaria a1 RegaloADomicilio
                   let v2 = a2 :+ 4
                   let v3 = VentaMultiple a3 3 Regalo
                   let v4 = a4 :+ 3
                   let f1 = FacturaMultiple [v1, v2, v3]
                   let f2 = FacturaUnitaria v1
                   let f3 = FacturaMultiple [v1, v2, v3, v4]
                   print $ precioArticulo a1 -- 3.536
                   print $ precioArticulo a2 -- 14.795
                   print $ precioArticulo a3  -- 28.374502
                   print $ precioVenta v1 -- 18.536
                   print $ precioVenta v2 -- 44.385
                   print $ precioVenta v3 -- 47.561753
                   print $ precioFactura f1 -- 110.48276
                   print $ precioFactura f2 -- 18.536
                   print $ precioFactura (f1 $+$ f2) --129.0187
                   print $ f1 $# a1
                   print $ show a1
                   print $ show a2
                   print $ show a3
                   print $ show v1
                   print $ show v2
                   print $ show v3
                   print $ show f1
                   print $ f1 !# a1 -- 1
                   print $ f1 !## [a1, a2] -- 5
                   print $ f1 -## a1 --
                   print $ f1 -% 3 --
                   print $ show f3 --
                   print $ eliminacionArticulosRepetidos f3 --

{--
    Resultado esperado:

        3.536
        14.795
        28.374502
        18.536
        44.385002
        47.561752
        110.48276
        18.536
        129.01875
        18.536
        "Producto basico: Tornillo con codigo 1 precio: 3.4 y con Ninguna promocion"
        "Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3"
        "Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50%"
        "Venta Regalo a domicilio=> Producto basico: Tornillo con codigo 1 precio: 3.4 y con Ninguna promocion"
        "Venta => Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3 comprado 4 veces"
        "Venta Regalo=> Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50% comprado 3 veces"
        "Venta Regalo a domicilio=> Producto basico: Tornillo con codigo 1 precio: 3.4 y con Ninguna promocion || Venta => Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3 comprado 4 veces || Venta Regalo=> 
        Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50% comprado 3 veces"
        1
        5
        Venta => Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3 comprado 4 veces || Venta Regalo=> Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50% comprado 3 veces
        Venta => Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3 comprado 4 veces || Venta Regalo=> Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50% comprado 3 veces
        "Venta Regalo a domicilio=> Producto basico: Tornillo con codigo 1 precio: 3.4 y con Ninguna promocion || Venta => Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3 comprado 4 veces || Venta Regalo=> 
        Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50% comprado 3 veces || Venta => Producto de lujo: Martillo con codigo 2 precio: 13.45 y con Ninguna promocion comprado 3 veces"
        Venta Regalo a domicilio=> Producto basico: Tornillo con codigo 1 precio: 3.4 y con Ninguna promocion comprado 1 veces || Venta Normal=> Producto importante: Martillo con codigo 2 precio: 13.45 y con Promocion 2x3 comprado 7 veces || Venta Regalo=> Producto de lujo: Taladradora con codigo 3 precio: 23.45 y con Promocion -50% comprado 3 veces
--}