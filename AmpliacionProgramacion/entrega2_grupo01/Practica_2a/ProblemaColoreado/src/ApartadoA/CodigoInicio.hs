{--
	Fichero: CodigoInicio.hs
	Autores: Aitor Melero, Arturo Morcillo
	Fecha: 29/10/2020
	Funcionalidad: Implementacion del codigoInicio de P2a.
--}

module ApartadoA.CodigoInicio where

-- Importamos el operador (//) de Data.List
import Data.List ((\\))

-- Declaracion del tipo Color como instancia de varias clases
data Color = Rojo | Verde | Azul deriving (Show,Enum,Eq)
-- Declaracion del tipo Provincia como instancia de varias clases
data Provincia = Al | Ca | Co | Gr | Ja | Hu | Ma | Se deriving (Show,Enum,Eq)
-- Creación del tipo sinonimo Frontera
type Frontera = Provincia -> [Provincia]
-- Funcion que especifica las provincias vecinas de cada provincia de Andalucia
frAndalucia :: Frontera
frAndalucia Al = [Gr]; frAndalucia Ca = [Hu,Se,Ma]
frAndalucia Co = [Se,Ma,Ja,Gr]; frAndalucia Gr = [Ma,Co,Ja,Al]
frAndalucia Ja = [Co,Gr]; frAndalucia Hu = [Ca,Se]
frAndalucia Ma = [Ca,Se,Co,Gr]; frAndalucia Se = [Hu,Ca,Ma,Co]
-- Declaracion del tipo Mapa
data Mapa = Atlas [Provincia] Frontera
-- Funcion que devuelve el mapa de Andalucia, devuelve sus provincias y sus fronteras
andalucia :: Mapa
andalucia = Atlas [Al .. Se] frAndalucia
-- Colores de provincias vecinas para un coloreado
coloresFrontera ::
    Provincia->[(Provincia,Color)]->Frontera-> [Color]
coloresFrontera provincia coloreado frontera
    = [col | (prov,col)<- coloreado, elem prov (frontera provincia)]
-- Posibles coloreados para un mapa y una lista de colores
coloreados :: (Mapa,[Color]) -> [[(Provincia,Color)]]
coloreados ((Atlas [] _), _) = [[]]
coloreados ((Atlas (prov:provs) frontera), colores)
    = [(prov,color):coloreado' |
        coloreado' <- coloreados
                            ((Atlas provs frontera), colores)
        , color <- colores \\
                    (coloresFrontera prov coloreado' frontera)]
-- Funcion que devuelve una lista con las provincias asociadas a un color
solucionColorear:: (Mapa,[Color]) -> [(Provincia,Color)]
solucionColorear = head . coloreados
sol1 = solucionColorear (andalucia, [Rojo .. Azul]) -- encuentra una solucion
sol2 = solucionColorear (andalucia, [Rojo,Verde]) -- sin solución

-----------------------------------------------------------------
-- Extra para Castilla y Leon
-----------------------------------------------------------------
-- Es el mismo codigo que el de Andalucia pero probado para Castilla y Leon
data Color' = Rojo' | Verde' | Amarillo' | Azul' deriving (Show,Enum,Eq)
data Provincia' = Le | Pa | Bu | So | Seg | Av | Sa | Za | Va deriving (Show,Enum,Eq)
type Frontera' = Provincia' -> [Provincia']
frCastillaLeon :: Frontera'
frCastillaLeon Le = [Za,Va,Pa]; frCastillaLeon Pa = [Le,Va,Bu]
frCastillaLeon Bu = [Pa,Va,Seg,So]; frCastillaLeon So = [Bu,Seg]
frCastillaLeon Seg = [So,Bu,Va,Av]; frCastillaLeon Av = [Seg,Va,Sa]
frCastillaLeon Sa = [Av,Va,Za]; frCastillaLeon Za = [Sa,Va,Le]
frCastillaLeon Va = [Le,Pa,Bu,Seg,Av,Sa,Za]
data Mapa' = Atlas' [Provincia'] Frontera'
castillaLeon :: Mapa'
castillaLeon = Atlas' [Le .. Va] frCastillaLeon
coloresFrontera' ::
    Provincia'->[(Provincia',Color')]->Frontera'-> [Color']
coloresFrontera' provincia coloreado frontera
    = [col | (prov,col)<- coloreado, elem prov (frontera provincia)]
coloreados' :: (Mapa',[Color']) -> [[(Provincia',Color')]]
coloreados' ((Atlas' [] _), _) = [[]]
coloreados' ((Atlas' (prov:provs) frontera), colores)
    = [(prov,color):coloreado' |
        coloreado' <- coloreados'
                            ((Atlas' provs frontera), colores)
        , color <- colores \\
                    (coloresFrontera' prov coloreado' frontera)]
solucionColorear':: (Mapa',[Color']) -> [(Provincia',Color')]
solucionColorear' = head . coloreados'
sol1' = solucionColorear' (castillaLeon, [Rojo' .. Azul'])
sol2' = solucionColorear' (castillaLeon, [Rojo',Verde'])