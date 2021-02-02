{--
	Fichero: E.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 21/10/2020
	Funcionalidad: Implementacion del apartado 'e' de P1b.
--}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoE.E where

import Data.List

-- Modificamos los data para tipos polimorficos
data Articulo a b where
    Articulo :: (Integral a, Fractional b) => a -> String -> b -> Articulo a b

data Venta a b c where
    VentaUnitaria :: (Integral a , Fractional b) => Articulo a b -> Venta a a b
    VentaMultiple :: (Integral a, Fractional b) => Articulo a b -> a -> Venta a a b
    (:+) :: (Integral a, Fractional b) => Articulo a b -> a -> Venta a a b

data Factura a b c where
    FacturaVacia :: (Integral a, Fractional b) => Factura a a b
    FacturaUnitaria :: (Integral a, Fractional b) => Venta a a b -> Factura a a b
    FacturaMultiple :: (Integral a, Fractional b) => [Venta a a b] -> Factura a a b

-- Funciones que actuan como getters
articulo :: (Integral a, Fractional b) => Venta a a b -> Articulo a b
articulo (VentaUnitaria a) = a
articulo (VentaMultiple a _) = a
articulo (a :+ _) = a

cantidad :: (Integral a, Fractional b) => Venta a a b -> a
cantidad (VentaUnitaria _) = 1
cantidad (VentaMultiple _ c) = c
cantidad (_ :+ c) = c

codigo :: (Integral a, Fractional b) => Articulo a b -> a
codigo (Articulo c _ _) = c

precio :: (Integral a, Fractional b) => Articulo a b -> b
precio (Articulo _ _ p) = p

nombre :: (Integral a, Fractional b) => Articulo a b -> String
nombre (Articulo _ n _) = n

-- Instancias de Show
instance (Integral a, Show a, Fractional b, Show b) => Show (Articulo a b) where 
    show a = "Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)

instance (Integral a, Show a, Fractional b, Show b) => Show (Venta a a b) where 
    show (VentaUnitaria a) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)
    show (VentaMultiple a c) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a) ++ " comprado " ++ show c ++ " veces"
    show (a :+ 1) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)
    show (a :+ c) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a) ++ " comprado " ++ show c ++ " veces"

instance (Integral a, Show a, Fractional b, Show b) => Show (Factura a a b) where 
    show FacturaVacia = "Factura Vacia"
    show (FacturaUnitaria v) = "Factura con una sola venta: " ++ show v
    show (FacturaMultiple [v]) = show v
    show (FacturaMultiple (v:vs)) = show v ++ " || " ++ show (FacturaMultiple vs)


precioVenta :: (Integral a, Fractional b) => Venta a a b -> b
precioVenta (VentaUnitaria a) = precio a
precioVenta (VentaMultiple a c) = precio a * fromIntegral c
precioVenta (a :+ 1) = precio a
precioVenta (a :+ c) = precio a * fromIntegral c

precioFactura :: (Integral a, Fractional b) => Factura a a b -> b
precioFactura FacturaVacia = 0
precioFactura (FacturaUnitaria v) = precioVenta v
precioFactura (FacturaMultiple [v]) = precioVenta v
precioFactura (FacturaMultiple (v:vs)) = foldr (+) 0 [precioVenta p | p <- v:vs]

-- fusionFacturas ($+$)
($+$) :: (Integral a, Fractional b) => Factura a a b -> Factura a a b -> Factura a a b
(FacturaUnitaria v1) $+$ FacturaVacia = FacturaUnitaria v1
FacturaVacia $+$ (FacturaUnitaria v2) = FacturaUnitaria v2
(FacturaMultiple v1) $+$ FacturaVacia = FacturaMultiple v1
FacturaVacia $+$ (FacturaMultiple v2) = FacturaMultiple v2
(FacturaUnitaria v1) $+$ (FacturaUnitaria v2) = FacturaMultiple [v1, v2]
(FacturaUnitaria v1) $+$ (FacturaMultiple v2) = FacturaMultiple (v1:v2)
(FacturaMultiple v1) $+$ (FacturaUnitaria v2) = FacturaMultiple (v2:v1)
(FacturaMultiple v1) $+$ (FacturaMultiple v2) = FacturaMultiple (v1 ++ v2)

-- Funcion necesaria por no tener deriving
sacarArticuloVenta :: (Integral a, Fractional b) => Venta a a b -> String
sacarArticuloVenta (VentaUnitaria a) = nombre a
sacarArticuloVenta (VentaMultiple a _) = nombre a
sacarArticuloVenta (a :+ _) = nombre a

-- precioTotalArticulo ($#)
($#) :: (Integral a, Fractional b) => Factura a a b -> Articulo a b -> b
(FacturaUnitaria v) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple [v]) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple (v:vs)) $# a = foldr (+) 0 [precioVenta p | p <- v:vs, sacarArticuloVenta p == nombre a]

-- ventasArticulo (!#)
(!#) :: (Integral a, Fractional b) => Factura a a b -> Articulo a b -> a
FacturaVacia !# _ = 0
(FacturaUnitaria v) !# a
            | sacarArticuloVenta v == nombre a = 1
            | otherwise = 0
(FacturaMultiple [v]) !# a
            | sacarArticuloVenta v == nombre a = cantidad v
            | otherwise = 0
(FacturaMultiple (v:vs)) !# a = cantidadVenta [v1 | v1 <- v:vs, sacarArticuloVenta v1 == nombre a]
                                where cantidadVenta [] = 0
                                      cantidadVenta [x] =  cantidad x
                                      cantidadVenta (x:_) =  cantidad x

-- ventasArticulos (!##)
(!##) :: (Integral a, Fractional b) => Factura a a b -> [Articulo a b] -> a
f !## [a] = f !# a 
f !## (a:as) = foldr (+) 0 [f !# a' | a' <- a:as]

-- eliminacionArticulos (-##)
(-##) :: (Integral a, Fractional b) => Factura a a b -> Articulo a b -> Factura a a b
FacturaVacia -## _ = FacturaVacia
(FacturaUnitaria v) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple [v]) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple (v:vs)) -## a = f'' f'
                        where f' = [ f | f <- v:vs, sacarArticuloVenta f /= nombre a]
                              f'' [] = FacturaVacia
                              f'' [x] = FacturaUnitaria x
                              f'' (x:xs) = FacturaMultiple (x:xs)

-- eliminacionCantida d (-%)
(-%) :: (Integral a, Fractional b) => Factura a a b -> a -> Factura a a b
FacturaVacia -% _ = FacturaVacia
(FacturaUnitaria v) -% n
    | cantidad v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple [v]) -% n
    | cantidad v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple (v:vs)) -% n = f'' f'
                        where f' = [ f | f <- v:vs, cantidad f >= n] 
                              f'' [] = FacturaVacia
                              f'' [x] = FacturaUnitaria x
                              f'' (x:xs) = FacturaMultiple (x:xs)

-- Funcion extra del apartado C
eliminacionArticulosRepetidos :: (Integral a, Fractional b) => Factura a a b -> Factura a a b
eliminacionArticulosRepetidos (FacturaMultiple (v:vs)) = FacturaMultiple (map sumarCantidad (agrupar (v:vs)))
                        where sumarCantidad :: (Integral a, Fractional b) => [Venta a a b] -> Venta a a b
                              sumarCantidad (v':vs') =  VentaMultiple (articulo v') (sum [cantidad x | x <- v':vs'])
                              agrupar :: (Integral a, Fractional b) => [Venta a a b] -> [[Venta a a b]]
                              agrupar (z:zs) = groupBy esCodigoIgual (ordenar (z:zs))
                              esCodigoIgual :: (Integral a, Fractional b) => Venta a a b -> Venta a a b ->Bool
                              esCodigoIgual v1 v2 = codigo (articulo v1) == codigo (articulo v2)
                              ordenar :: (Integral a, Fractional b) => [Venta a a b] -> [Venta a a b]
                              ordenar [] = []
                              ordenar (x:xs) = ordenar [ y | y <- xs, codigo (articulo (head xs)) <= codigo (articulo x)] ++ [x] ++ ordenar [ z | z <- xs, codigo (articulo (head xs)) > codigo (articulo x)]

-- Main
mainApartadoE :: IO ()
mainApartadoE = do let a1 = Articulo 1 "Tornillo" 3.4
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
        70.35
        100.65
        3.4
        104.05000000000001
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