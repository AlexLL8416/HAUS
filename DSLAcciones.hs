module DSLAcciones where

import Fisicas 
import Entidades
import Logica

data BotAction =
    SetVelocidad Vector  -- Fijar velocidad
    |AddVelocidad Vector -- Suma velocidad
    |Stop                -- Para
    |Disparar            -- Dispara una bala en el ángulo del cañón
    |GirarCannon Angle   -- Gira el cañón
    |Nada                -- No hace nada
    deriving (Show,Eq)

{-- Función que aplica una acción a un tanque
apliAccion :: Tanque -> BotAction -> Tanque
apliAccion t accion =
    case accion of
        SetVelocidad v -> updateVelocity "SetVelocidad" v t
        AddVelocidad v -> updateVelocity "AddVelocidad" v t
        Stop           -> updateVelocity "Stop" (0,0) t 
        GirarCannon a  -> t {extra = (extra t) {anguloCannon = a}}
        Disparar       -> t
        Nada           -> t-}

apliAccion :: Tanque -> BotAction -> Tanque
apliAccion t accion =
    case accion of
        SetVelocidad v -> updateVelocity "SetVelocidad" v t
        AddVelocidad v -> updateVelocity "AddVelocidad" v t
        Stop           -> updateVelocity "Stop" (0,0) t 
        GirarCannon a  -> fmap (\info -> info { anguloCannon = a }) t
        Disparar       -> t
        Nada           -> t


-- Función que genera una bala con una velocidad fija
fireBullet:: Tanque -> Bala
fireBullet t =
    let a = anguloCannon (extra t)
        p = posicion t
        v = ((cos a) * 10,(sin a) * 10)
        info = InfoBala {danyo = 10}
        in Objeto {posicion = p, velocidad = v, tamanyo = (5,10), extra = info} -- Bala en dirección del ángulo del cañon con una velocidad de 10 (Se podría modifica)

-- Ejemplo de bot --
-- Bot simple que se mueve hasta detectar a otro tanque, entonces se para, gira su cañon y dispara
-- Para que tuvieras posibilidades de ganar debería tener un gran rango de rada
-- Si no detecta ningún enemigo avanza hacia arriba a la derecha, debería spawnear cerca de la esquina inferior izquierda
botSimple :: Mundo -> Tanque -> [BotAction]
botSimple mundo bot =
    case [enemigo|enemigo<-tanques mundo, enemigo /= bot, detectedAgent bot enemigo] of
        (enemigo:_) ->
            [Stop,
            GirarCannon (angleToTarget (posicion bot) (posicion enemigo)),
            Disparar
            ]
        [] -> [SetVelocidad (1,1)]
