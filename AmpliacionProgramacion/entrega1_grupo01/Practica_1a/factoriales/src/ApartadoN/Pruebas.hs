{--
	Fichero: Pruebas.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 09/09/2020
	Funcionalidad: Modulo que llama a las diferentes
				   funciones de Factoriales.hs para los tests.
--}

module ApartadoN.Pruebas where

import Test.QuickCheck
-- definido en el paquete oculto QuickCheck
-- se accede al paquete declarándolo en “package.yaml”
import ApartadoA.Func.Factoriales
import ApartadoI.Func.Factoriales
import ApartadoK.Func.Factoriales
import ApartadoL.Func.Factoriales
import ApartadoM.Func.Factoriales


-- como fact7 y fact8 son las funciones dadas y estan bien
-- las utilizaremos para probar el resto de funciones.

-- apartado i
prueba1 :: Integer -> Bool
prueba1 n = fact7_noArgs1 n == fact7 n
prueba2 :: Integer -> Bool
prueba2 n = fact7_noArgs2 n == fact7 n

-- apartado k
prueba3 :: Integer -> Bool
prueba3 n = fact8b n == fact8 n

-- apartado l
prueba4 :: Integer -> Bool
prueba4 n
    | n < 0 = fact7_maybe n == Nothing
    | otherwise = fact7_maybe n == Just (fact7 n)
prueba5 :: Integer -> Bool
prueba5 n
    | n < 0 = fact8_maybe n == Nothing
    | otherwise = fact8_maybe n == Just (fact8 n)

-- apartado m
prueba6 :: Integer -> Bool
prueba6 n
    | n < 0 = fact7_poli n == Nothing
    | otherwise = fact7_poli n == Just (fact7 n)
prueba7 :: Integer -> Bool
prueba7 n
    | n < 0 = fact8_poli n == Nothing
    | otherwise = fact8_poli n == Just (fact8 n)


-- llamadas a pruebas
comp1 :: IO ()
comp1= quickCheck prueba1
    where valores = [n | n<-[0..150]]

comp2 :: IO ()
comp2= quickCheck prueba2
    where valores = [n | n<-[0..150]]

comp3 :: IO ()
comp3= quickCheck prueba3
    where valores = [n | n<-[0..150]]

comp4 :: IO ()
comp4= quickCheck prueba4
    where valores = [n | n<-[-50..100]]

comp5 :: IO ()
comp5= quickCheck prueba5
    where valores = [n | n<-[-50..100]]

comp6 :: IO ()
comp6= quickCheck prueba6
    where valores = [n | n<-[-50..100]]

comp7 :: IO ()
comp7= quickCheck prueba7
    where valores = [n | n<-[-50..100]]

-- main
pruebas :: IO ()
pruebas = do comp1
             comp2
             comp3
             comp4
             comp5
             comp6
             comp7

{--
    Resultado esperado:

        +++ OK, passed 100 tests.
        +++ OK, passed 100 tests.
        +++ OK, passed 100 tests.
        +++ OK, passed 100 tests.
        +++ OK, passed 100 tests.
        +++ OK, passed 100 tests.
        +++ OK, passed 100 tests.
--}