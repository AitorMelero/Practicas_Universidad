{--
	Fichero: Ejemplos.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo que llama a las diferentes
				   funciones de Factoriales.hs.
--}

module ApartadoL.Ejemplos where

-- importamos las funciones de factoriales
import ApartadoL.Func.Factoriales

-- imprimos ejemplos de factoriales
ejemplos4 :: IO ()
ejemplos4 = do
	print ("El resultado de (fact7_maybe 4) es: " ++ (show (fact7_maybe 4)))
	print ("El resultado de (fact7_maybe -1) es: " ++ (show (fact7_maybe (1-2))))
	print ("El resultado de (fact7_maybe 6) es: " ++ (show (fact7_maybe 6)))
	print ("El resultado de (fact8_maybe 4) es: " ++ (show (fact8_maybe 4)))
	print ("El resultado de (fact8_maybe -1) es: " ++ (show (fact8_maybe (1-2))))
	print ("El resultado de (fact8_maybe 6) es: " ++ (show (fact8_maybe 6)))

{--
	Resultado esperado:
        
        "El resultado de (fact7_maybe 4) es: Just 24"
		"El resultado de (fact7_maybe -1) es: Nothing"
		"El resultado de (fact7_maybe 6) es: Just 720"
		"El resultado de (fact8_maybe 4) es: Just 24"
		"El resultado de (fact8_maybe -1) es: Nothing"
		"El resultado de (fact8_maybe 6) es: Just 720"
		 
--}