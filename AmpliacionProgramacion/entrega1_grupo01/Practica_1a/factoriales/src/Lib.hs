module Lib
    ( someFunc
    ) where

import ApartadoA.Ejemplos
import ApartadoI.Ejemplos
import ApartadoK.Ejemplos
import ApartadoL.Ejemplos
import ApartadoM.Ejemplos
import ApartadoN.Pruebas

someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = do
    ejemplos
    ejemplos2
    ejemplos3
    ejemplos4
    ejemplos5
    pruebas
