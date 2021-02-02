{--
	Fichero: Ejemplos.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo que llama a las diferentes
				   funciones de Factoriales.hs.
--}

module ApartadoI.Ejemplos where

-- importamos las funciones de factoriales
import ApartadoI.Func.Factoriales

-- imprimos ejemplos de factoriales
ejemplos2 :: IO ()
ejemplos2 = do
	print ("El resultado de (fact7_noArgs1 4) es: " ++ (show (fact7_noArgs1 4)))
	print ("El resultado de (fact7_noArgs1 5) es: " ++ (show (fact7_noArgs1 5)))
	print ("El resultado de (fact7_noArgs1 6) es: " ++ (show (fact7_noArgs1 6)))
	print ("El resultado de (fact7_noArgs2 4) es: " ++ (show (fact7_noArgs2 4)))
	print ("El resultado de (fact7_noArgs2 5) es: " ++ (show (fact7_noArgs2 5)))
	print ("El resultado de (fact7_noArgs2 6) es: " ++ (show (fact7_noArgs2 6)))

{--
	Resultado esperado:
		
		"El resultado de (fact7_noArgs1 4) 4 es: 24"
		"El resultado de (fact7_noArgs1 5) 5 es: 120"
		"El resultado de (fact7_noArgs1 6) 6 es: 720"
		"El resultado de (fact7_noArgs2 4) 4 es: 24"
		"El resultado de (fact7_noArgs2 5) 5 es: 120"
		"El resultado de (fact7_noArgs2 6) 6 es: 720"
		 
--}