-- ================================
-- Instancias Functor y Applicative
-- ================================
import Entidades
import Fisicas (distanceBetween, Vector)
--para pegar al final de Entidades.hs

instance Functor Objeto where
  fmap f (Objeto pos vel tam ext) = Objeto pos vel tam (f ext)

instance Applicative Objeto where
  pure x = Objeto (0,0) (0,0) (0,0) x
  (Objeto _ _ _ f) <*> (Objeto pos vel tam x) = Objeto pos vel tam (f x)


-- Este ejemplo demuestra cómo usar fmap para actualizar la energía de todos los tanques de forma declarativa 
-- añadir en Logica.hs

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


-- en Colisiones.hs

normalesAristas :: Rect -> [Vector]
normalesAristas (a, b, c, d) =
    fmap (normalizar . perp . uncurry sub) [(b, a), (c, b), (d, c), (a, d)]
  
proyectar :: Rect -> Vector -> (Float, Float)
proyectar (a, b, c, d) v =
    let nv = normalizar v
        ps = fmap (`dot` nv) [a, b, c, d]
    in (minimum ps, maximum ps)

checkCollision :: Rect -> Rect -> Bool
checkCollision r1 r2 =
    or $ fmap (checkCollisionEje r1 r2) norms
  where
    norms = nub (normalesAristas r1 ++ normalesAristas r2)

detectRobotProjectileCollisions :: Mundo -> [(Tanque, Bala)]
detectRobotProjectileCollisions Mundo { tanques = ts, balas = bs } =
    filter (uncurry (\t b -> checkCollision (rectTanque t) (rectBala b))) ((,) <$> ts <*> bs)

detectRobotRobotCollisions :: Mundo -> [(Tanque, Tanque)]
detectRobotRobotCollisions Mundo { tanques = ts } =
    filter (uncurry (\t1 t2 -> checkCollision (rectTanque t1) (rectTanque t2))) (combinaciones ts)

checkCollisions :: Mundo -> [Colision]
checkCollisions m =
  (uncurry TanqueTanque <$> tt) ++ (uncurry TanqueBala <$> tb)
  where
    tt = detectRobotRobotCollisions m
    tb = detectRobotProjectileCollisions m


-- en DSLAcciones.hs

apliAccion :: Tanque -> BotAction -> Tanque
apliAccion t accion =
    case accion of
        SetVelocidad v -> updateVelocity "SetVelocidad" v t
        AddVelocidad v -> updateVelocity "AddVelocidad" v t
        Stop           -> updateVelocity "Stop" (0,0) t 
        GirarCannon a  -> fmap (\info -> info { anguloCannon = a }) t
        Disparar       -> t
        Nada           -> t
