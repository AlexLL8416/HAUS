module DSLAcciones where

import Fisicas (addVV,subVec)
import Entidades (Accion, posicion)
import Data.Fixed (mod')
import Acciones

-- Definición de bots

-- Bot que sigue al primer enemigo que vea
botSimple :: Accion ()
botSimple = do
    enems <- enemigos
    case enems of
        (enemigo:_) -> do
            girarHacia (posicion enemigo)
            acelerar

            preparado <- apuntarCanyonHacia (posicion enemigo)
            if preparado then disparar else nada
        _ -> parar
    evitarParedes

-- Bot que da vueltas sobre si mismo
botVueltas :: Accion ()
botVueltas = do
    yo <- miTanque
    t <- tiempo
    let angulo = t `mod'` (2 * pi)

    mantenerVelocidad 50
    girarHacia (addVV (posicion yo) (cos angulo, sin angulo))

-- Bot que se queda quieto y dispara con precisión. Necesitaría un mayor radar
botFrancotirador :: Accion ()
botFrancotirador = do
    enems <- enemigos
    case enems of
        (enemigo:_) -> do
            preparado <- apuntarCanyonHacia (posicion enemigo)
            if preparado then disparar else nada
        _ -> nada

-- Bot al detectar un enemigo se aleja de él mientras dispara
botEvasivo :: Accion ()
botEvasivo = do
    yo <- miTanque
    enems <- enemigos
    case enems of
        (enemigo:_) -> do
            let posE = posicion enemigo
                posY = posicion yo
                dirEscape = addVV posY (subVec posY posE)
            girarHacia dirEscape
            mantenerVelocidad 80
            preparado <- apuntarCanyonHacia posE
            if preparado then disparar else nada
        _ -> parar
    evitarParedes
