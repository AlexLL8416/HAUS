module Logica (
      countActiveRobots
    , ejecutarTanques
    , updateTanque
    , detectedAgent
    , isRobotAlive
    , updateMundo
) where

import Entidades
import Fisicas
import Constantes
import Colisiones



-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar
detectedAgent :: Tanque -> Tanque -> Bool
detectedAgent (Objeto {posicion = pos_a, extra = a}) (Objeto {posicion = pos_b}) = distanceBetween pos_a pos_b <= radar a

-- isRobotAlive: True si la energía del robot es mayor a 0
isRobotAlive :: Tanque -> Bool
isRobotAlive Objeto {extra = a} = energia a > 0

-- countActiveRobots: Contar los robots que están vivos
countActiveRobots :: [Tanque] -> Int
countActiveRobots tanques = length [t | t <- tanques, isRobotAlive t]

-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo
updatePosition :: Float -> Objeto a -> Objeto a
updatePosition dt t = t {posicion = (px + (vx * dt), py + (vy * dt))}
    where
        (px, py) = posicion t
        (vx, vy) = velocidad t

-- Aplica movimiento angular al tanque para acercarse al ángulo objetivo
updateTanqueAngulo :: Float -> Tanque -> Tanque
updateTanqueAngulo dt = fmap (\x -> x { angulo = acercarAngulo (dt * velGiroTanque) (angulo x) (quiereAngulo x) })

-- Aplica movimiento angular al cañón para acercarse al ángulo objetivo
updateTanqueAnguloCannon :: Float -> Tanque -> Tanque
updateTanqueAnguloCannon dt = fmap (\x -> x { anguloCannon = acercarAngulo (dt * velGiroCannon) (anguloCannon x) (quiereAnguloCannon x) })

-- Aplica la aceleración y el frenado al tanque según la dirección a la que está mirando
updateTanqueVelocity :: Float -> Tanque -> Tanque
updateTanqueVelocity dt t = let
    info = extra t
    dir = (cos (angulo info), sin (angulo info))  -- Calculamos el vector al que el tanque está mirando
    magVelAct = modV (velocidad t)                -- Calculamos la magnitud de la velocidad actual del tanque
    deltaVel
      | acelerando info = magnitudAceleracion       -- Si el tanque está acelerando, le añadimos la constante de aceleración a la velocidad
      | frenando info = magVelAct * factorFrenado   -- Si el tanque está frenando, multiplicamos la velocidad actual por un factor constante
      | otherwise = 0
    magVel = min (magVelAct + (deltaVel * dt)) maxTanqueVelocity  -- Calculamos la magnitud del nuevo vector velocidad, teniendo en cuenta la aceleración que pide el tanque y la velocidad máxima permitida
    adjVel = mulVS magVel dir                                     -- Calculamos el nuevo vector velocidad dirigiendolo en la dirección a la que mira el tanque
    in t { velocidad = adjVel }

-- Actualia la física de un tanque
updateTanque :: Float -> Tanque -> Tanque
updateTanque dt t
  -- Si el tanque no tiene energía, no se mueve
  | energia (extra t) <= 0 = t
  | otherwise = foldl (\ac f -> f dt ac) t
        [ updateTanqueAngulo
        , updateTanqueAnguloCannon
        , updateTanqueVelocity
        , updatePosition
        ]

-- Ejecuta la mónada Accion sobre un mundo de parte de un tanque, y devuelve el mundo mutado
ejecutarAccion :: Int -> Mundo -> Accion () -> Mundo
ejecutarAccion tidx m (Accion f) =
    let (_, m') = f tidx m
    in m'

-- Ejecuta las acciones de todos los tanques de un mundo
ejecutarTanques :: Mundo -> Mundo
ejecutarTanques m = ejecutarTanques' 0 (tanques m) m

ejecutarTanques' :: Int -> [Tanque] -> Mundo -> Mundo
ejecutarTanques' _ [] m = m
ejecutarTanques' tidx (t:ts) m =
    let m' = if isRobotAlive t then ejecutarAccion tidx m (cerebro (extra t)) else m
    in ejecutarTanques' (tidx + 1) ts m'

updateMundo :: Float -> Mundo -> Mundo
updateMundo dt m =
    let
        -- Mover tanques
        tsMovidos = map (updateTanque dt) (tanques m)

        -- Ejecutar acciones de los tanques
        mConAcciones = ejecutarTanques m { tanques = tsMovidos }

        tsAccionados = tanques mConAcciones
        bsExistentes = balas mConAcciones
        explsExistentes = explosiones mConAcciones

        -- Mover balas
        bsMovidas = map (updatePosition dt) bsExistentes

        -- Detectar colisiones entre balas y tanques
        colisionesTB = [(b, t) | b <- bsMovidas, t <- tsAccionados,
                                 idObjeto t /= disparador (extra b),
                                 checkCollision (rectTanque t) (rectBala b)]

        -- Actualizar la energía de los tanques golpeados
        tsActualizados = map (\t ->
            let balasImpacto = [b | (b, t') <- colisionesTB, idObjeto t' == idObjeto t]
                totalDanyo = sum (map (danyo . extra) balasImpacto)
                infoNueva = (extra t) { energia = max 0 (energia (extra t) - totalDanyo) }
            in t { extra = infoNueva }
          ) tsAccionados

        -- Eliminar balas que impactaron
        bsFinales = filter (\b -> not $ any (\(b', _) -> idObjeto b' == idObjeto b) colisionesTB) bsMovidas

        -- Crear nuevas explosiones para cada bala que impactó
        nuevasExplosiones = [ addExplosion (posicion b) 15 | (b, _) <- colisionesTB ]

        -- Actualizar explosiones existentes (reducir tiempo de vida)
        updateExplosion dt = fmap (\x -> x { tiempoVida = tiempoVida x - dt })

        explsActualizadas = filter (\e -> tiempoVida (extra e) > 0) $
                            map (updateExplosion dt) (explsExistentes ++ nuevasExplosiones)

        -- Actualizar el tiempo de juego
        tiempo = tiempoJuego m + dt

    in mConAcciones {
           tanques = tsActualizados
         , balas = bsFinales
         , explosiones = explsActualizadas
         , tiempoJuego = tiempo
         }
