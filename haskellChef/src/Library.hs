module Library where
import PdePreludat
import Data.Int

-- PARTE A 

-- Tipos
data Participante = UnParticipante {
    nombre :: String,
    trucosDeCocina :: [Truco],
    platoEspecialidad :: Plato
} deriving (Show)

data Plato = UnPLato {
    dificultad :: Number,
    componentes :: [Componente]
} deriving (Show, Eq)

data Componente = UnComponente {
    ingrediente :: String,
    pesoEnGramos :: Number
} deriving (Show, Eq)

type Truco = Plato -> Plato

-- Trucos
agregarComponente :: String -> Number -> Truco
agregarComponente unIngrediente cantidadEnGr unPlato = unPlato { componentes = (UnComponente unIngrediente cantidadEnGr) : componentes unPlato }
-- me hubiese gustado revisar si ya tenia el ingrediente para que en ese caso le agregue esos gramos pero
-- no se me ocurrio como filtar para modificar ESE ingrediente

endulzar :: Number -> Truco
endulzar cantidadDeAzucar unPlato = agregarComponente "azucar" cantidadDeAzucar unPlato

salar :: Number -> Truco
salar cantidadDeSal unPlato = agregarComponente "sal" cantidadDeSal unPlato

darSabor :: Number -> Number -> Truco
darSabor cantidadDeSal cantidadDeAzucar unPlato = salar cantidadDeSal . endulzar cantidadDeAzucar $ unPlato

duplicarCantidad :: Componente -> Componente
duplicarCantidad unComponente = unComponente { pesoEnGramos = 2 * pesoEnGramos unComponente }

duplicarPorcion :: Truco
duplicarPorcion unPlato = unPlato { componentes = map duplicarCantidad . componentes $ unPlato }

esUnBardo :: Plato -> Bool
esUnBardo unPlato = dificultad unPlato > 7 && (length . componentes $ unPlato) > 5

modificarDificultad :: Number -> Plato -> Plato
modificarDificultad nuevaDificultad unPlato = unPlato {dificultad = max 0 . min 10 $ nuevaDificultad}

menosGramosQue :: Number -> Componente -> Bool
menosGramosQue unaCantidad unComponente = pesoEnGramos unComponente < unaCantidad

quitarComponentes :: Plato -> Plato
quitarComponentes unPlato = unPlato {componentes = filter (menosGramosQue 10) . componentes $ unPlato} 

simplificar :: Plato -> Plato
simplificar unPlato
    | esUnBardo unPlato = modificarDificultad 5 . quitarComponentes $ unPlato
    | otherwise = unPlato

-- Funciones de platos
esComponente :: String -> Componente -> Bool
esComponente unIngrediente unComponente = ingrediente unComponente == unIngrediente

esAlimentoAnimal :: Componente -> Bool
esAlimentoAnimal unComponente = esComponente "carne" unComponente || 
                                esComponente "huevo" unComponente || 
                                esComponente "queso" unComponente || 
                                esComponente "leche" unComponente || 
                                esComponente "manteca" unComponente 

esVegano :: Plato -> Bool
esVegano unPlato = null . filter esAlimentoAnimal . componentes $ unPlato

esSinTacc :: Plato -> Bool
esSinTacc unPlato = null . filter (esComponente "harina") . componentes $ unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = dificultad unPlato > 7 && (length . componentes $ unPlato) > 5

hayCantidadIngrediente :: Number -> Componente -> Bool
hayCantidadIngrediente unaCantidad unComponente = unaCantidad <= pesoEnGramos unComponente

noAptoHipertension :: Plato -> Bool 
noAptoHipertension unPlato = not . null . filter (\componente -> hayCantidadIngrediente 2 componente && esComponente "sal" componente) . componentes $ unPlato

-- PARTE B

platoDePepe = UnPLato 8 [UnComponente "sal" 5]
pepe = UnParticipante "Pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] platoDePepe

-- PARTE C

aplicarTruco :: Truco -> Plato -> Plato
aplicarTruco unTruco unPlato = unTruco unPlato 

cocinar :: Participante -> Plato
cocinar unParticipante = foldr aplicarTruco (platoEspecialidad unParticipante) (trucosDeCocina unParticipante)
--                 funcion que aplica el resto | plato original (primer caso) | lista de trucos (funciones aplicadas al plato)

pesoDeComponente :: Componente -> Number
pesoDeComponente unComponente = pesoEnGramos unComponente

pesoDelPlato :: Plato -> Number
pesoDelPlato unPlato =foldr (+) 0 (map pesoDeComponente (componentes unPlato))

compararConOperacion :: (Plato -> Number) -> Plato -> Plato -> Bool
compararConOperacion unaOperacion plato1 plato2 = unaOperacion plato1 > unaOperacion plato2
-- no se si esta astraccion era necesaria o no

esMasDificil1que2 :: Plato -> Plato -> Bool
esMasDificil1que2 plato1 plato2 = compararConOperacion dificultad plato1 plato2

esMasPesado1que2 :: Plato -> Plato -> Bool
esMasPesado1que2 plato1 plato2 = compararConOperacion pesoDelPlato plato1 plato2

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = esMasDificil1que2 plato1 plato2 && esMasPesado1que2 plato1 plato2

-- falta: de una lista de participantes elegir el que tiene el mejor plato despues de aplicarles cocinar
mejorPlatoCocinado ::  Participante -> Participante -> Participante
mejorPlatoCocinado part1 part2
    | esMejorQue (cocinar part1) (cocinar part2) = part1
    |otherwise = part2
    -- no sabia como hacer con empates asi que ante un empate hice que gane el 2do

participanteEstrella :: [Participante] -> Participante
participanteEstrella unosParticipantes = foldr1 mejorPlatoCocinado unosParticipantes