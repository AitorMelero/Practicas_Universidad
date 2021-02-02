{--
	Fichero: Factoriales.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo en el que se definen diferentes
				   maneras de implementar el factorial.
--}

module ApartadoA.Func.Factoriales where

-- funcion de pliegue
fact7 :: Integer -> Integer
fact7 n = 
		foldr (*) 1 [1..n]
-- composicion de funciones
fact8 = product . enumFromTo 1