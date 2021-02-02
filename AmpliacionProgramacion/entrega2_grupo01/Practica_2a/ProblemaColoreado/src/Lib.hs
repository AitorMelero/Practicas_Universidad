module Lib
    ( someFunc
    ) where

import ApartadoA.A
import ApartadoB.B
import ApartadoC.C
import ApartadoD.D
import ApartadoE.E

someFunc :: IO ()
someFunc = do mainApartadoA
              mainApartadoB
              mainApartadoC
              mainApartadoD
              mainApartadoE