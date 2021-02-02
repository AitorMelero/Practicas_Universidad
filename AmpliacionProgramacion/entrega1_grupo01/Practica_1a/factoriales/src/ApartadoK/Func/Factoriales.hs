{--
	Fichero: Factoriales.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo en el que se definen diferentes
				   maneras de implementar el factorial.
--}

module ApartadoK.Func.Factoriales where

-- apartado k
fact8b :: Integer -> Integer
fact8b n = product [1..n]