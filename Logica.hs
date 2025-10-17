module Logica where

import Entidades
import Fisicas (distanceBetween, Vector)

-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar
detectedAgent :: Tanque -> Tanque -> Bool
detectedAgent (Objeto {posicion = pos_a, extra = a}) (Objeto {posicion = pos_b}) = distanceBetween pos_a pos_b <= radar a

-- isRobotAlive: True si la energía del robot es mayor a 0
isRobotAlive :: Tanque -> Bool
isRobotAlive Objeto {extra = a} = energia a > 0

{-- countActiveRobots: Contar los robots que están vivos
countActiveRobots :: [Tanque] -> Int
countActiveRobots tanques = length [t|t <- tanques, isRobotAlive t]-}

-- updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada
-- Ya no se utiliza porque al generalizar la velocidad para todo (tanque, bala, explosión), se puede juntar todo en una misma función

{-- updateVelocity: Actualizar velocidad basada en la acción de movimiento
updateVelocity :: String -> Vector -> Objeto a -> Objeto a
updateVelocity accion vel t =
    case accion of
        "SetVelocidad" -> t {velocidad = vel}   
        "AddVelocidad" -> t {velocidad = (vx + fst vel, vy + snd vel)}   -- Le suma la velocidad indicada a la velocidad actual
            where (vx, vy) = velocidad t -- Vector actual de la velocidad del tanque
        "Stop" -> t {velocidad = (0, 0)}
        _ -> t -}

{-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo
updatePosition :: Objeto a -> Float -> Objeto a
updatePosition t dt = t {posicion = (px + (vx * dt), py + (vy * dt))}
    where 
        (px, py) = posicion t
        (vx, vy) = velocidad t-}

-- Tarea 4.3 --

-- Resta 10 puntos de energía a todos los tanques
quitarEnergia :: [Tanque] -> [Tanque]
quitarEnergia = fmap (fmap (\info -> info { energia = energia info - 10 }))


-- Cambiar el nombre de un tanque usando Applicative
renombrarTanque :: Tanque -> Tanque
renombrarTanque t =
  pure (\info -> info { nombre = "Tanque Pro" }) <*> t


updatePosition :: Objeto a -> Float -> Objeto a
updatePosition t dt = 
    (\(px, py) (vx, vy) -> t { posicion = (px + vx * dt, py + vy * dt) })
    <$> pure (posicion t)
    <*> pure (velocidad t)


updateVelocity :: String -> Vector -> Objeto a -> Objeto a
updateVelocity accion vel t =
    case accion of
        "SetVelocidad" -> t { velocidad = vel }
        "AddVelocidad" -> (\(vx, vy) -> t { velocidad = (vx + fst vel, vy + snd vel) })
                            <$> pure (velocidad t)
        "Stop"         -> t { velocidad = (0, 0) }
        _              -> t

countActiveRobots :: [Tanque] -> Int
countActiveRobots = length . filter id . fmap isRobotAlive