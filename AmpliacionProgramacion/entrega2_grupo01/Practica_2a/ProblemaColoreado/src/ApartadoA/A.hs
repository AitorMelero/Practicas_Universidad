{--
	Fichero: A.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 29/10/2020
	Funcionalidad: Implementacion del mainApartadoA de P2a.
--}

module ApartadoA.A where

-- Importamos el codigoInicio
import ApartadoA.CodigoInicio

-- mainApartadoA
mainApartadoA :: IO ()
mainApartadoA = do -- Creamos los colores
                   let rojo = Rojo
                   let verde = Verde
                   let azul = Azul
                   -- Creamos las provincias
                   let almeria = Al
                   let cadiz = Ca
                   let cordoba = Co
                   let granada = Gr
                   let jaen = Ja
                   let huelva = Hu
                   let malaga = Ma
                   let sevilla = Se
                   -- Mostramos las fronteras de cada provincia
                   putStrLn "Las fronteras son:"
                   print $ "Almeria: " ++ show (frAndalucia Al)
                   print $ "Cadiz: " ++ show (frAndalucia Ca)
                   print $ "Cordoba: " ++ show (frAndalucia Co)
                   print $ "Granada: " ++ show (frAndalucia Gr)
                   print $ "Jaen: " ++ show (frAndalucia Ja)
                   print $ "Huelva: " ++ show (frAndalucia Hu)
                   print $ "Malaga: " ++ show (frAndalucia Ma)
                   print $ "Sevilla: " ++ show (frAndalucia Se)
                   putStrLn ""
                   -- Creamos el mapa de andalucia
                   let andalucia = Atlas [Al .. Se] frAndalucia
                   -- Mostramos que se puede asociar un color a cada provincia usando 3 colores
                   putStrLn "Los coloreados con rojo, verde y azul son:"
                   print $ coloreados (andalucia, [Rojo .. Azul])
                   -- Mostramos que no se puede asociar un color a cada provincia usando 3 colores
                   putStrLn "Los coloreados con rojo y azul son:"
                   print $ coloreados (andalucia, [Rojo, Azul])
                   putStrLn ""
                   -- Solucion del problema
                   putStrLn "Las soluciones son:"
                   print $ sol1 -- Con solucion
                   --print $ sol2 -- Sin solucion, Exception: empty list
                   putStrLn ""
                   -------------------------------------------
                   -- Probamos ahora con Castilla y Leon
                   -------------------------------------------
                   -- Mostramos las fronteras de cada provincia
                   putStrLn "Las fronteras de Castilla y Leon son:"
                   print $ "Leon: " ++ show (frCastillaLeon Le)
                   print $ "Palencia: " ++ show (frCastillaLeon Pa)
                   print $ "Burgos: " ++ show (frCastillaLeon Bu)
                   print $ "Soria: " ++ show (frCastillaLeon So)
                   print $ "Segovia: " ++ show (frCastillaLeon Seg)
                   print $ "Avila: " ++ show (frCastillaLeon Av)
                   print $ "Salamanca: " ++ show (frCastillaLeon Sa)
                   print $ "Zamora: " ++ show (frCastillaLeon Za)
                   print $ "Valladolid: " ++ show (frCastillaLeon Va)
                   putStrLn ""
                   -- Creamos el mapa de andalucia
                   let castillaLeon = Atlas' [Le .. Va] frCastillaLeon
                   -- Mostramos que se puede asociar un color a cada provincia
                   --putStrLn "Los coloreados con rojo, verde, amarillo y azul son:"
                   --print $ coloreados' (castillaLeon, [Rojo' .. Azul']) -- Infinito
                   -- Mostramos que no se puede asociar un color a cada provincia usando 3 colores
                   --putStrLn "Los coloreados con rojo y azul son:"
                   --print $ coloreados' (castillaLeon, [Rojo', Azul']) -- No hay combinacion
                   putStrLn ""
                   -- Solucion del problema
                   putStrLn "Las soluciones para Castilla y Leon son:"
                   print $ sol1' -- Con solucion
                   print $ sol2' -- Sin solucion, Exception: empty list

{--
    Resultado esperado:

    Las fronteras son:
    "Almeria: [Gr]"
    "Cadiz: [Hu,Se,Ma]"
    "Cordoba: [Se,Ma,Ja,Gr]"
    "Granada: [Ma,Co,Ja,Al]"
    "Jaen: [Co,Gr]"
    "Huelva: [Ca,Se]"
    "Malaga: [Ca,Se,Co,Gr]"
    "Sevilla: [Hu,Ca,Ma,Co]"

    Los coloreados con rojo, verde y azul son:
    [[(Al,Verde),(Ca,Azul),(Co,Azul),(Gr,Rojo),(Ja,Verde),(Hu,Verde),(Ma,Verde),(Se,Rojo)],[(Al,Azul),(Ca,Azul),(Co,Azul),(Gr,Rojo),(Ja,Verde),(Hu,Verde),(Ma,Verde),(Se,Rojo)],[(Al,Verde),(Ca,Verde),(Co,Verde),(Gr,Rojo),(Ja,Azul),(Hu,Azul),(Ma,Azul),(Se,Rojo)],[(Al,Azul),(Ca,Verde),(Co,Verde),(Gr,Rojo),(Ja,Azul),(Hu,Azul),(Ma,Azul),(Se,Rojo)],[(Al,Rojo),(Ca,Azul),(Co,Azul),(Gr,Verde),(Ja,Rojo),(Hu,Rojo),(Ma,Rojo),(Se,Verde)],[(Al,Azul),(Ca,Azul),(Co,Azul),(Gr,Verde),(Ja,Rojo),(Hu,Rojo),(Ma,Rojo),(Se,Verde)],[(Al,Rojo),(Ca,Rojo),(Co,Rojo),(Gr,Verde),(Ja,Azul),(Hu,Azul),(Ma,Azul),(Se,Verde)],[(Al,Azul),(Ca,Rojo),(Co,Rojo),(Gr,Verde),(Ja,Azul),(Hu,Azul),(Ma,Azul),(Se,Verde)],[(Al,Rojo),(Ca,Verde),(Co,Verde),(Gr,Azul),(Ja,Rojo),(Hu,Rojo),(Ma,Rojo),(Se,Azul)],[(Al,Verde),(Ca,Verde),(Co,Verde),(Gr,Azul),(Ja,Rojo),(Hu,Rojo),(Ma,Rojo),(Se,Azul)],[(Al,Rojo),(Ca,Rojo),(Co,Rojo),(Gr,Azul),(Ja,Verde),(Hu,Verde),(Ma,Verde),(Se,Azul)],[(Al,Verde),(Ca,Rojo),(Co,Rojo),(Gr,Azul),(Ja,Verde),(Hu,Verde),(Ma,Verde),(Se,Azul)]]
    Los coloreados con rojo y azul son:
    []

    Las soluciones son:
    [(Al,Verde),(Ca,Azul),(Co,Azul),(Gr,Rojo),(Ja,Verde),(Hu,Verde),(Ma,Verde),(Se,Rojo)]

    Las fronteras de Castilla y Leon son:
    "Leon: [Za,Va,Pa]"
    "Palencia: [Le,Va,Bu]"
    "Burgos: [Pa,Va,Seg,So]"
    "Soria: [Bu,Seg]"
    "Segovia: [So,Bu,Va,Av]"
    "Avila: [Seg,Va,Sa]"
    "Salamanca: [Av,Va,Za]"
    "Zamora: [Sa,Va,Le]"
    "Valladolid: [Le,Pa,Bu,Seg,Av,Sa,Za]"


    Las soluciones para Castilla y Leon son:
    [(Le,Azul'),(Pa,Amarillo'),(Bu,Verde'),(So,Rojo'),(Seg,Amarillo'),(Av,Verde'),(Sa,Amarillo'),(Za,Verde'),(Va,Rojo')]
    *** Exception: Prelude.head: empty list
--}