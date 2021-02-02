{--
	Fichero: Ejemplos.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 24/09/2020
	Funcionalidad: Modulo que llama a las diferentes
				   funciones de Factoriales.hs.
--}

module ApartadoM.Ejemplos where

-- importamos las funciones de factoriales
import ApartadoM.Func.Factoriales

-- imprimos ejemplos de factoriales
ejemplos5 :: IO ()
ejemplos5 = do
    print ("El resultado de (fact7_poli -1) es: " ++ (show (fact7_poli (1-2))))
    print ("El resultado de (fact7_poli 5) es: " ++ (show (fact7_poli 5)))
    print ("El resultado de (fact7_poli 6) es: " ++ (show (fact7_poli 6)))
    print ("El resultado de (fact8_poli -1) es: " ++ (show (fact8_poli (1-2))))
    print ("El resultado de (fact8_poli 5) es: " ++ (show (fact8_poli 5)))
    print ("El resultado de (fact8_poli 6) es: " ++ (show (fact8_poli 6)))

{--
	Resultado esperado:
        
        "El resultado de (fact7_poli -1) es: Nothing"
		"El resultado de (fact7_poli 5) es: Just 120"
		"El resultado de (fact7_poli 6) es: Just 720"
		"El resultado de (fact8_poli -1) es: Nothing"
		"El resultado de (fact8_poli 5) es: Just 120"
		"El resultado de (fact8_poli 6) es: Just 720"
		 
--}