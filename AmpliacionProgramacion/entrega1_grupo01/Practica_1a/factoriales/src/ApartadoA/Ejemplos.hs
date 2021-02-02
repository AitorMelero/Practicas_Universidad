{--
	Fichero: Ejemplos.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo que llama a las diferentes
				   funciones de Factoriales.hs.
--}

module ApartadoA.Ejemplos where

-- importamos las funciones de factoriales
import ApartadoA.Func.Factoriales

-- imprimos ejemplos de factoriales
ejemplos :: IO ()
ejemplos = do
	print ("El resultado de (fact7 4) es: " ++ (show (fact7 4)))
	print ("El resultado de (fact7 5) es: " ++ (show (fact7 5)))
	print ("El resultado de (fact7 6) es: " ++ (show (fact7 6)))
	print ("El resultado de (fact8 4) es: " ++ (show (fact8 4)))
	print ("El resultado de (fact8 5) es: " ++ (show (fact8 5)))
	print ("El resultado de (fact8 6) es: " ++ (show (fact8 6)))

{--
	Resultado esperado:
		
		"El resultado de (fact7) 4 es: 24"
		"El resultado de (fact7) 5 es: 120"
		"El resultado de (fact7) 6 es: 720"
		"El resultado de (fact8) 4 es: 24"
		"El resultado de (fact8) 5 es: 120"
		"El resultado de (fact8) 6 es: 720"
		 
--}