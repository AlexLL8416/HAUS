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

-- Detección y estado de los robots --

detectedAgent :: Tanque -> Tanque -> Bool
detectedAgent (Objeto {posicion = pos_a, extra = a}) (Objeto {posicion = pos_b}) =
    distanceBetween pos_a pos_b <= radar a

isRobotAlive :: Tanque -> Bool
isRobotAlive Objeto {extra = a} = energia a > 0

countActiveRobots :: [Tanque] -> Int
countActiveRobots tanques = length [t | t <- tanques, isRobotAlive t]

-- Actualización de física y movimiento --

updatePosition :: Float -> Objeto a -> Objeto a
updatePosition dt t = t { posicion = (px + (vx * dt), py + (vy * dt)) }
  where
    (px, py) = posicion t
    (vx, vy) = velocidad t

updateTanqueAngulo :: Float -> Tanque -> Tanque
updateTanqueAngulo dt = fmap (\x -> x { angulo = acercarAngulo (dt * velGiroTanque) (angulo x) (quiereAngulo x) })

updateTanqueAnguloCannon :: Float -> Tanque -> Tanque
updateTanqueAnguloCannon dt = fmap (\x -> x { anguloCannon = acercarAngulo (dt * velGiroCannon) (anguloCannon x) (quiereAnguloCannon x) })

updateTanqueVelocity :: Float -> Tanque -> Tanque
updateTanqueVelocity dt t =
  let info = extra t
      dir = (cos (angulo info), sin (angulo info))
      magVelAct = modV (velocidad t)
      deltaVel
        | acelerando info = magnitudAceleracion
        | frenando info   = magVelAct * factorFrenado
        | otherwise       = 0
      magVel = min (magVelAct + (deltaVel * dt)) maxTanqueVelocity
      adjVel = mulVS magVel dir
  in t { velocidad = adjVel }

updateTanque :: Float -> Tanque -> Tanque
updateTanque dt t
  | energia (extra t) <= 0 = t
  | otherwise = foldl (\ac f -> f dt ac) t
        [ updateTanqueAngulo
        , updateTanqueAnguloCannon
        , updateTanqueVelocity
        , updatePosition
        ]

-- Limitar posición dentro del campo --

limitarPosicion :: Size -> Tanque -> Tanque
limitarPosicion (ancho, alto) t = t { posicion = (nx, ny), velocidad = (vx', vy') }
  where
    (x, y) = posicion t
    (vx, vy) = velocidad t
    halfX = ancho / 2
    halfY = alto / 2
    minX = -halfX
    maxX =  halfX
    minY = -halfY
    maxY =  halfY
    nx = clamp x minX maxX
    ny = clamp y minY maxY
    vx' | nx == minX || nx == maxX = 0 | otherwise = vx
    vy' | ny == minY || ny == maxY = 0 | otherwise = vy

clamp :: Float -> Float -> Float -> Float
clamp v minV maxV = max minV (min v maxV)

-- Ejecución de acciones --

ejecutarAccion :: Int -> Mundo -> Accion () -> Mundo
ejecutarAccion tidx m (Accion f) =
    let (_, m') = f tidx m
    in m'

ejecutarTanques :: Mundo -> Mundo
ejecutarTanques m = ejecutarTanques' 0 (tanques m) m

ejecutarTanques' :: Int -> [Tanque] -> Mundo -> Mundo
ejecutarTanques' _ [] m = m
ejecutarTanques' tidx (t:ts) m =
    let m' = ejecutarAccion tidx m (cerebro (extra t))
    in ejecutarTanques' (tidx + 1) ts m'

-- Actualización del mundo (colisiones, explosiones, etc.) --

updateMundo :: Float -> Mundo -> Mundo
updateMundo dt m =
    let
        -- 1. Mover tanques
        tsMovidos = map (updateTanque dt) (tanques m)
        tsLimitados = map (limitarPosicion (tamanyoMundo m)) tsMovidos
        mBase = m { tanques = tsLimitados }

        -- 2. Ejecutar acciones de los tanques
        mConAcciones = ejecutarTanques mBase
        tsAccionados = tanques mConAcciones
        bsExistentes = balas mConAcciones
        explsExistentes = explosiones mConAcciones

        -- 3. Mover balas
        bsMovidas = map (updatePosition dt) bsExistentes

        -- 4. Colisiones bala-tanque
        colisionesTB =
          [ (b, t)
          | b <- bsMovidas
          , t <- tsAccionados
          , idObjeto t /= disparador (extra b)
          , checkCollision (rectTanque t) (rectBala b)
          ]

        tsDañados = map (\t ->
            let balasImpacto = [b | (b, t') <- colisionesTB, idObjeto t' == idObjeto t]
                totalDanyo = sum (map (danyo . extra) balasImpacto)
                infoNueva  = (extra t) { energia = max 0 (energia (extra t) - totalDanyo) }
            in t { extra = infoNueva }
          ) tsAccionados

        bsFinales = filter (\b -> not (any (\(b', _) -> idObjeto b' == idObjeto b) colisionesTB)) bsMovidas
        explosionesImpacto = [ addExplosion (posicion b) 10 | (b, _) <- colisionesTB ]

        -- 5. Colisiones entre tanques (daño leve)
        colisionesTT =
          [ (t1, t2)
          | t1 <- tsDañados
          , t2 <- tsDañados
          , idObjeto t1 /= idObjeto t2
          , checkCollision (rectTanque t1) (rectTanque t2)
          ]

        tsTrasColision = map (\t ->
            let hayImpacto = any (\(a,b) -> idObjeto a == idObjeto t || idObjeto b == idObjeto t) colisionesTT
                danyoChoque = if hayImpacto then 1 else 0  -- solo 1 punto por choque leve
                infoNueva = (extra t) { energia = max 0 (energia (extra t) - danyoChoque) }
            in t { extra = infoNueva }
          ) tsDañados

        -- 6. Tanques destruidos y explosiones
        tanquesMuertos = filter (\t -> energia (extra t) <= 0) tsTrasColision
        explosionesMuerte = [ addExplosion (posicion t) 30 | t <- tanquesMuertos ]
        tsVivos = filter (\t -> energia (extra t) > 0) tsTrasColision

        -- 7. Actualizar explosiones
        updateExplosion dt = fmap (\x -> x { tiempoVida = tiempoVida x - dt })
        explsActualizadas =
          filter (\e -> tiempoVida (extra e) > 0)
            (map (updateExplosion dt) (explsExistentes ++ explosionesImpacto ++ explosionesMuerte))

        -- 8. Tiempo del juego
        tiempoNuevo = tiempoJuego m + dt

    in mConAcciones
         { tanques = map (limitarPosicion (tamanyoMundo m)) tsVivos
         , balas = bsFinales
         , explosiones = explsActualizadas
         , tiempoJuego = tiempoNuevo
         }
