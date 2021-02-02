{--
	Fichero: Factoriales.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo en el que se definen diferentes
				   maneras de implementar el factorial.
--}

module ApartadoM.Func.Factoriales where

fact7_poli :: (Num a, Enum a, Ord a) => a -> Maybe a
fact7_poli n
  | n < 0 = Nothing
  | otherwise = Just (foldr (*) 1 [1..n])

fact8_poli :: (Num a, Enum a, Ord a) => a -> Maybe a
fact8_poli = f
	where f n
		| n < 0 = Nothing
		| otherwise =  Just (product (enumFromTo 1 n))
