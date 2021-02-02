{--
	Fichero: Factoriales.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo en el que se definen diferentes
				   maneras de implementar el factorial.
--}

module ApartadoI.Func.Factoriales where

-- apartado i
fact7_noArgs1 = foldr (*) 1 . enumFromTo 1

fact7_noArgs2 = f
	where f n = 
		foldr (*) 1 [1..n]