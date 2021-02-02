{--
	Fichero: Ejemplos.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo que llama a las diferentes
				   funciones de Factoriales.hs.
--}

module ApartadoK.Ejemplos where

-- importamos las funciones de factoriales
import ApartadoK.Func.Factoriales

-- imprimos ejemplos de factoriales
ejemplos3 :: IO ()
ejemplos3 = do
	print ("El resultado de (fact8b 4) es: " ++ (show (fact8b 4)))
	print ("El resultado de (fact8b 5) es: " ++ (show (fact8b 5)))
	print ("El resultado de (fact8b 6) es: " ++ (show (fact8b 6)))

{--
	Resultado esperado:
		
		"El resultado de (fact8b) 4 es: 24"
		"El resultado de (fact8b) 5 es: 120"
		"El resultado de (fact8b) 6 es: 720"
		 
--}