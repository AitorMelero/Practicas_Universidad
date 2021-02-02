{--
	Fichero: C.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 19/10/2020
	Funcionalidad: Implementacion del apartado 'c' de P1b.
--}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, TypeSynonymInstances #-}

module ApartadoG.G where

import Data.List

type Cantidad = Int
          
data Articulo = Articulo {codigo::Int, nombre::String, precio::Float}
data Venta = VentaUnitaria Articulo | VentaMultiple Articulo Cantidad | Articulo :+ Cantidad
data Factura = FacturaVacia | FacturaUnitaria Venta | FacturaMultiple Venta [Factura] -- FacturaVacia y FacturaUnitaria son las hojas

instance Show Articulo where 
    show a = "Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)

instance Show Venta where 
    show (VentaUnitaria a) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)
    show (VentaMultiple a c) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a) ++ " comprado " ++ show c ++ " veces"
    show (a :+ 1) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a)
    show (a :+ c) = "Venta => Articulo " ++ nombre a ++ " con codigo " ++ show (codigo a) ++ " y precio: " ++ show (precio a) ++ " comprado " ++ show c ++ " veces"

instance Show Factura where 
    show FacturaVacia = "Factura Vacia"
    show (FacturaUnitaria v) = show v
    show (FacturaMultiple v []) = show v
    show (FacturaMultiple v [f]) = show v ++ " || " ++ show f
    show (FacturaMultiple v (f:fs)) = show v ++ " || " ++ show f ++ " || " ++ show fs
    

getVentasFactura :: Factura -> [Venta]
getVentasFactura FacturaVacia = []
getVentasFactura (FacturaUnitaria v) = [v]
getVentasFactura (FacturaMultiple v []) = [v]
getVentasFactura (FacturaMultiple v [f]) = v:getVentasFactura f
getVentasFactura (FacturaMultiple v (f:fs)) = [v] ++ getVentasFactura f ++ getVentasFacturaAux fs
    where getVentasFacturaAux [] = []
          getVentasFacturaAux (f:fs) = getVentasFactura f ++ getVentasFacturaAux fs

construirArbol :: [Venta] -> Factura
construirArbol [] = FacturaVacia
construirArbol [v] = FacturaUnitaria v
construirArbol (v:vs) = FacturaMultiple v (hacerFacturas vs)
    where hacerFacturas [v] = [FacturaUnitaria v]
          hacerFacturas (v:vs) = FacturaUnitaria v : hacerFacturas vs

precioVenta :: Venta -> Float
precioVenta (VentaUnitaria a) = precio a
precioVenta (VentaMultiple a c) = precio a * fromIntegral c
precioVenta (a :+ 1) = precio a
precioVenta (a :+ c) = precio a * fromIntegral c

precioFactura :: Factura -> Float
precioFactura FacturaVacia = 0
precioFactura (FacturaUnitaria v) = precioVenta v
precioFactura (FacturaMultiple v []) = precioVenta v
precioFactura (FacturaMultiple v [f]) = precioVenta v + precioFactura f
--CAMBIO 1
precioFactura (FacturaMultiple v (f:fs)) = foldr (+) (precioVenta v) [precioFactura p | p <- f:fs]
-- fusionFacturas ($+$)
($+$) :: Factura -> Factura -> Factura
-- 2 vacias
FacturaVacia $+$ FacturaVacia = FacturaVacia
-- Unitaria y vacia
(FacturaUnitaria v1) $+$ FacturaVacia = FacturaUnitaria v1
FacturaVacia $+$ (FacturaUnitaria v2) = FacturaUnitaria v2
-- Vacia y multiple con un elemento
(FacturaMultiple v1 [f]) $+$ FacturaVacia = FacturaMultiple v1 [f]
FacturaVacia $+$ (FacturaMultiple v2 [f]) = FacturaMultiple v2 [f]
-- Vacia y multiple con varios elementos
(FacturaMultiple v1 (f:fs)) $+$ FacturaVacia = FacturaMultiple v1 (f:fs)
FacturaVacia $+$ (FacturaMultiple v2 (f:fs)) = FacturaMultiple v2 (f:fs)
-- Dos unitarias
(FacturaUnitaria v1) $+$ (FacturaUnitaria v2) = FacturaMultiple v1 [FacturaUnitaria v2]
-- Unitaria y multiple con un elementos
(FacturaUnitaria v1) $+$ (FacturaMultiple v2 [f]) = FacturaMultiple v2 (FacturaUnitaria v1:[f])
(FacturaMultiple v1 [f]) $+$ (FacturaUnitaria v2) = FacturaMultiple v1 (FacturaUnitaria v2:[f])
-- Unitaria y multiple con varios elementos
(FacturaUnitaria v1) $+$ (FacturaMultiple v2 (f:fs)) = FacturaMultiple v2 (FacturaUnitaria v1:(f:fs))
(FacturaMultiple v1 (f:fs)) $+$ (FacturaUnitaria v2) = FacturaMultiple v1 (FacturaUnitaria v2:(f:fs))
-- Dos multiples con un elemento
(FacturaMultiple v1 [f1]) $+$ (FacturaMultiple v2 [f2]) = FacturaMultiple v1 (FacturaMultiple v2 [f2]:[f1])
-- Multiple con un elemento y multiple con varios elementos
(FacturaMultiple v1 (f:fs)) $+$ (FacturaMultiple v2 [f2]) = FacturaMultiple v1 (FacturaMultiple v2 [f2]:(f:fs))
(FacturaMultiple v1 [f1]) $+$ (FacturaMultiple v2 (f:fs)) = FacturaMultiple v2 (FacturaMultiple v1 [f1]:(f:fs))
-- Dos multiples con dos elementos
(FacturaMultiple v1 (f1:fs1)) $+$ (FacturaMultiple v2 (f2:fs2)) = FacturaMultiple v1 (FacturaMultiple v2 (f2:fs2):(f1:fs1))


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
FacturaVacia $# a = 0

(FacturaUnitaria v) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple v []) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v
            | otherwise = 0
(FacturaMultiple v [f]) $# a
            | sacarArticuloVenta v == nombre a = precioVenta v + f $# a
            | otherwise = 0

-- CAMBIO 2
(FacturaMultiple v (f:fs)) $# a = foldr (+) 0 [precioVenta p | p <- getVentasFactura (FacturaMultiple v (f:fs)), sacarArticuloVenta p == nombre a]


-- ventasArticulo (!#)
(!#) :: Factura -> Articulo -> Int
FacturaVacia !# _ = 0
(FacturaUnitaria v) !# a
            | sacarArticuloVenta v == nombre a = 1
            | otherwise = 0
(FacturaMultiple v []) !# a
            | sacarArticuloVenta v == nombre a = cantidadVenta v
            | otherwise = 0
            where
                  cantidadVenta (VentaUnitaria _) = 1
                  cantidadVenta (VentaMultiple _ c) = c
                  cantidadVenta (_ :+ c) = c
(FacturaMultiple v [f]) !# a
    | sacarArticuloVenta v == nombre a = cantidadVenta v + f !# a
    | otherwise = f !# a
    where
          cantidadVenta (VentaUnitaria _) = 1
          cantidadVenta (VentaMultiple _ c) = c
          cantidadVenta (_ :+ c) = c
      
-- CAMBIO 3
(FacturaMultiple v (f:fs)) !# a = cantidadVenta [v1 | v1 <- getVentasFactura (FacturaMultiple v (f:fs)), sacarArticuloVenta v1 == nombre a]
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
(FacturaMultiple v []) -## a
    | sacarArticuloVenta v == nombre a = FacturaVacia
    | otherwise = FacturaUnitaria v
(FacturaMultiple v [f]) -## a = construirArbol (eliminarArticulos (getVentasFactura (FacturaMultiple v [f])) a)
    where eliminarArticulos [] _ = []
          eliminarArticulos [v] a
                | sacarArticuloVenta v == nombre a = []
                | otherwise = [v]
          eliminarArticulos (v:vs) a = f'' f'
                        where f' = [ f | f <- v:vs, sacarArticuloVenta f /= nombre a]
                              f'' [] = []
                              f'' [x] = [x]
                              f'' (x:xs) = x:xs

(FacturaMultiple v (f:fs)) -## a = construirArbol (eliminarArticulos (getVentasFactura (FacturaMultiple v (f:fs))) a)
    where eliminarArticulos [] _ = []
          eliminarArticulos [v] a
                | sacarArticuloVenta v == nombre a = []
                | otherwise = [v]
          eliminarArticulos (v:vs) a = f'' f'
                        where f' = [ f | f <- v:vs, sacarArticuloVenta f /= nombre a]
                              f'' [] = []
                              f'' [x] = [x]
                              f'' (x:xs) = x:xs



-- eliminacionCantidad (-%)
(-%) :: Factura -> Int -> Factura
FacturaVacia -% _ = FacturaVacia
(FacturaUnitaria v) -% n
    | cantidadVenta v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
    where
          cantidadVenta (VentaUnitaria _) = 1
          cantidadVenta (VentaMultiple _ c) = c
          cantidadVenta (_ :+ c) = c
(FacturaMultiple v []) -% n
    | cantidadVenta v < n = FacturaVacia
    | otherwise = FacturaUnitaria v
    where
          cantidadVenta (VentaUnitaria _) = 1
          cantidadVenta (VentaMultiple _ c) = c
          cantidadVenta (_ :+ c) = c

(FacturaMultiple v [f]) -% n = construirArbol (eliminarcantidad (getVentasFactura (FacturaMultiple v [f]))n)
            where 
                eliminarcantidad [] _ = []
                eliminarcantidad [v] n
                    | cantidadVenta v < n = []
                    | otherwise = [v]
                eliminarcantidad (v:vs) n = f'' f'
                    where f' = [ f | f <- v:vs, cantidadVenta f >= n]
                          f'' [] = []
                          f'' [x] = [x]
                          f'' (x:xs) = x:xs
                cantidadVenta (VentaUnitaria _) = 1
                cantidadVenta (VentaMultiple _ c) = c
                cantidadVenta (_ :+ c) = c
-- CAMBIO 6

-- Funcion extra del apartado C
eliminacionArticulosRepetidos :: Factura -> Factura
eliminacionArticulosRepetidos (FacturaMultiple v (f:fs)) = construirArbol (map sumarCantidad (agrupar (getVentasFactura (FacturaMultiple v (f:fs)))))
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
mainApartadoG :: IO ()
mainApartadoG = do let a1 = Articulo 1 "Tornillo" 3.4
                   let a2 = Articulo 2 "Martillo" 13.45
                   let a3 = Articulo 3 "Taladradora" 23.45
                   let a4 = Articulo 2 "Martillo" 13.45
                   let v1 = VentaUnitaria a1
                   let v2 = a2 :+ 2
                   let v3 = VentaMultiple a3 3
                   let v4 = a4 :+ 3
                   let f1 = FacturaVacia
                   let f2 = FacturaUnitaria v1
                   let f3 = FacturaMultiple v2 [f2]
                   let f4 = FacturaMultiple v3 [f1, f3]
                   let f5 = FacturaMultiple v4 [f1, f4]
                   print $ precioVenta v1
                   print $ precioVenta v2
                   print $ precioVenta v3
                   print $ show "FACTURAS: "
                   print $ precioFactura f1
                   print $ precioFactura f2
                   print $ precioFactura f3
                   print $ precioFactura f4
                   print $ precioFactura f5
                   print $ precioFactura (f1 $+$ f2)
                   print $ precioFactura (f3 $+$ f4)
                   print $ precioFactura (f3 $+$ f5)
                   print $ show "operadores"
                   print $ f1 $# a1
                   print $ f2 $# a1
                   print $ f3 $# a1
                   print $ f4 $# a1
                   print $ f4 $# a3
                   print $ f5 $# a3
                   print $ show "Otros operadores"
                   -- print $ show a1
                   -- print $ show a2
                   -- print $ show a3
                   -- print $ show v1
                   -- print $ show v2
                   -- print $ show v3
                   -- print $ show f1
                   -- print $ show f2
                   -- print $ show f3
                   print $ f2 !# a1
                   print $ f1 !## [a1, a2]
                   print $ f4 -## a1
                   print $ f1 -% 3
                   print $ show "articulos repes a fuera"
                   print $ show f3
                   print $ eliminacionArticulosRepetidos f3
                  

{--
    Resultado esperado:

        3.4
        26.9
        70.350006
        "\"FACTURAS: \""
        0.0
        3.4
        30.3
        100.65001
        141.0
        3.4
        130.95001
        171.3
        "\"operadores\""
        0.0
        3.4
        0.0
        3.4
        70.350006
        70.350006
        "\"Otros operadores\""
        1
        0
        Venta => Articulo Taladradora con codigo 3 y precio: 23.45 comprado 3 veces || Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces
        Factura Vacia
        "\"articulos repes a fuera\""
        "Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces || Venta => Articulo Tornillo con codigo 1 y precio: 3.4"
        Venta => Articulo Tornillo con codigo 1 y precio: 3.4 comprado 1 veces || Venta => Articulo Martillo con codigo 2 y precio: 13.45 comprado 2 veces
--}