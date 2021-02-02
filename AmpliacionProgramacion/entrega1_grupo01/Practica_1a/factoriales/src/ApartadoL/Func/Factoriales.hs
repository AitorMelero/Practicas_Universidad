{--
	Fichero: Factoriales.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo en el que se definen diferentes
				   maneras de implementar el factorial.
--}

module ApartadoL.Func.Factoriales where

fact7_maybe :: Integer -> Maybe Integer
fact7_maybe n 
  | n < 0 = Nothing
  | otherwise = Just (foldr (*) 1 [1..n])

fact8_maybe :: Integer -> Maybe Integer
fact8_maybe = f
	where f n
		| n < 0 = Nothing
		| otherwise =  Just (product (enumFromTo 1 n))

