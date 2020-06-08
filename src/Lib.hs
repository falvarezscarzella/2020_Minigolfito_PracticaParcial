module Lib where
import Text.Show.Functions

laVerdad = True

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

tiroEjemplo = UnTiro 50 20 10

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


--Punto 1
--a)
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad= UnTiro {velocidad= 10, precision= precisionJugador habilidad * 2, altura= 0 }

madera :: Palo
madera habilidad= UnTiro {velocidad= 100, precision= precisionJugador habilidad `div` 2, altura= 5}

hierro :: Int -> Palo
hierro n habilidad= UnTiro {velocidad= fuerzaJugador habilidad * n, precision= precisionJugador habilidad `div` n, altura= max (n-3) 0}

--b)
losPalos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

--Punto 2

golpe :: Palo -> Jugador -> Tiro
golpe palo= palo.habilidad

--Punto 3
data Obstaculo = UnObstaculo{
    efecto:: EfectoObstaculo,
    condicion:: CondicionObstaculo
}

type CondicionObstaculo = Tiro -> Bool
type EfectoObstaculo = Tiro -> Tiro

superacionObstaculo :: Obstaculo -> Tiro -> Tiro
superacionObstaculo obstaculo tiro | (condicion obstaculo) tiro = (efecto obstaculo) tiro
                                   | otherwise = anularTiro tiro

anularTiro :: Tiro -> Tiro
anularTiro tiro = tiro{velocidad=0, precision=0, altura=0}

rampita :: Obstaculo
rampita = UnObstaculo efectoRampita condicionRampita

efectoRampita :: Tiro -> Tiro
efectoRampita tiro = tiro{velocidad= velocidad tiro * 2, precision= 100, altura= 0}

condicionRampita :: Tiro -> Bool
condicionRampita tiro = (precision tiro > 90) && (altura tiro == 0)

laguna :: Int -> Obstaculo
laguna largoLaguna = UnObstaculo (efectoLaguna largoLaguna) condicionLaguna                        

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoLaguna tiro =  tiro{altura= altura tiro * largoLaguna }

condicionLaguna :: CondicionObstaculo
condicionLaguna tiro = (velocidad tiro > 80) && ( between 1 5 (altura tiro))

hoyo :: Obstaculo
hoyo = UnObstaculo anularTiro condicionHoyo 

condicionHoyo :: CondicionObstaculo
condicionHoyo tiro = (between 5 20 (velocidad tiro)) && (altura tiro == 0) && (precision tiro > 95)

--Punto 4

--a
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (sirvePalo obstaculo jugador) losPalos 

sirvePalo :: Obstaculo -> Jugador -> Palo -> Bool
sirvePalo obstaculo jugador palo= (condicion obstaculo) (golpe palo jugador)

--b
tirosConsecutivos :: [Obstaculo] -> Tiro -> Int
tirosConsecutivos [] _ = 0
tirosConsecutivos (obstaculo : obstaculos) tiroInicial | (condicion obstaculo) tiroInicial = 1 + tirosConsecutivos obstaculos (efecto obstaculo tiroInicial) 
                                                       | otherwise = 0

--c
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos =  maximoSegun (tirosConsecutivos obstaculos.flip golpe jugador ) losPalos 


--Punto 5

type Tabla = [(Jugador,Puntos)]
type Puntuacion = (Jugador,Puntos)

padresDelNinoQueNoGano :: Tabla -> [String]
padresDelNinoQueNoGano tablaDePuntos = map (padre.fst) . filter ((elGanador tablaDePuntos /=) . snd) $ tablaDePuntos

elGanador :: Tabla -> Puntos
elGanador = maximum . map snd 