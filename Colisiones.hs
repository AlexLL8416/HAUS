-- Código de las colisiones

module Colisiones where

import Data.List (nub)
import Fisicas
import Entidades
import Utilidades

-- Representa una posible colisión entre dos tanques o una colisión entre un tanque y una bala
data Colision = TanqueTanque Tanque Tanque | TanqueBala Tanque Bala

-- Devuelve un Rect representando la hitbox de una bala por defecto
hitboxBala :: Rect
hitboxBala = ((0, 0), (0, 10), (5, 10), (5, 0))

-- Devuelve un Rect representando la hitbox de un tanque por defecto
hitboxTanque :: Rect
hitboxTanque = ((0, 0), (0, 100), (100, 100), (100, 0))

{-- Calcula el vector normal de cada arista de un Rect
normalesAristas :: Rect -> [Vector]
normalesAristas (a, b, c, d) =
    [
        normalizar $ perp $ sub b a,
        normalizar $ perp $ sub c b,
        normalizar $ perp $ sub d c,
        normalizar $ perp $ sub a d
    ]-}

-- Calcula la proyección de un rectangulo sobre un eje, retornando su valor minimo y maximo
{-
Ejemplo:
                  |
        /------/  ‖ < min
       /      /   ‖
      /      /    ‖ => proyección sobre (0, 1)
     /      /     ‖
    /------/      ‖ < max
                  |
----===========--  => proyección sobre (1, 0)
    ^         ^
   min       max
-}
{-proyectar :: Rect -> Vector -> (Float, Float)
proyectar (a, b, c, d) v = (minimum ps, maximum ps)    -- proyectamos cada vértice en el eje y nos quedamos con el menor y el mayor
    where nv = normalizar v
          ps = [dot x nv | x <- [a, b, c, d]]-}

-- Devuelve True si las proyecciones de los rectángulos se solapan en el eje dado
-- Ejemplo de no solapamiento:
--   p1: ------==========------------
--   p2: ------------------=======---
--
-- Ejemplo de solapamiento:
--   p1: ------==========------------
--   p2: ------------========--------
checkCollisionEje :: Rect -> Rect -> Vector -> Bool
checkCollisionEje r1 r2 v = (p2min <= p1max) && (p1min <= p2max)
    where (p1min, p1max) = proyectar r1 v
          (p2min, p2max) = proyectar r2 v

{-- Devuelve True si los rectángulos están colisionando
checkCollision :: Rect -> Rect -> Bool
checkCollision r1 r2 = or [checkCollisionEje r1 r2 v | v <- norms]  -- Hay colisión si hay solapamiento en cualquiera de los ejes, siendo los ejes las normales de las aristas
    where norms = nub (normalesAristas r1 ++ normalesAristas r2)    -- Lista de aristas de los dos Rect, quitando duplicados -}

-- Calcula la hitbox de un tanque teniendo en cuenta su posición y rotación
rectTanque :: Tanque -> Rect
rectTanque Objeto { posicion = p, extra = a } = getVertices (trasladoRect hitboxTanque p) (anguloCannon a)

-- Calcula la hitbox de una bala teniendo en cuenta su posición y dirección a la que se dirige
rectBala :: Bala -> Rect
rectBala Objeto { posicion = p, velocidad = v } = getVertices (trasladoRect hitboxBala p) (angleToTarget (0, 0) v)

-- Detecta todas las colisiones que se están produciendo entre tanques y balas y devuelve cada par (tanque, bala) que están
{-- colisionando entre sí
detectRobotProjectileCollisions :: Mundo -> [(Tanque, Bala)]
detectRobotProjectileCollisions Mundo { tanques = ts, balas = bs } = [(t, b) | t <- ts, b <- bs, checkCollision (rectTanque t) (rectBala b)]-}

-- Detecta todas las colisiones que se están produciendo entre tanques y devuelve cada par (tanque, tanque) que están
{-- colisionando entre sí
detectRobotRobotCollisions :: Mundo -> [(Tanque, Tanque)]
detectRobotRobotCollisions Mundo { tanques = ts } = [(t1, t2) | (t1, t2) <- combinaciones ts, checkCollision (rectTanque t1) (rectTanque t2)]-}

-- Detecta todas las colision que se están produciendo y devuelve un dato Colision representando cada colisión entre dos tanques
{-- o entre un tanque y una bala
checkCollisions :: Mundo -> [Colision]
checkCollisions m = [TanqueTanque t1 t2 | (t1, t2) <- tt] ++ [TanqueBala t b | (t, b) <- tb]
    where tt = detectRobotRobotCollisions m
          tb = detectRobotProjectileCollisions m-}

-- Tarea 4.3

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