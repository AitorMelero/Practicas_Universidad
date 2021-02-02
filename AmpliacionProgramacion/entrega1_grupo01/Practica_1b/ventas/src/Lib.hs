module Lib
    ( someFunc
    ) where

-- Importamos todos los modulos
import ApartadoA.A
import ApartadoB.B
import ApartadoC.C
import ApartadoD.D
import ApartadoE.E
import ApartadoF.F
import ApartadoG.G
import ApartadoH.H

someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = do mainApartadoA
              mainApartadoB
              mainApartadoC
              mainApartadoD
              mainApartadoE
              mainApartadoF
              mainApartadoG
              mainApartadoH
