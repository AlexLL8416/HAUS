module Renderizado (dibujarMundo, Recursos(..)) where

import Graphics.Gloss
import Entidades
import Logica (countActiveRobots)
import Fisicas (rad2deg)
import Constantes (duracionExplosiones)

-- =====================================================
-- ESTRUCTURA DE RECURSOS (imágenes cargadas)
-- =====================================================

data Recursos = Recursos
  { imgTanque     :: Picture
  , imgTorreta    :: Picture
  , imgBala       :: Picture
  , imgExplosion  :: Picture
  , imgFondo      :: Picture
  }

-- =====================================================
-- FUNCIÓN PRINCIPAL DE RENDERIZADO
-- =====================================================

dibujarMundo :: Recursos -> Mundo -> Picture
dibujarMundo recursos m =
  Pictures
    [ dibujarFondo recursos
    , Pictures (map (dibujarTanque recursos) (tanques m))
    , Pictures (map (dibujarBala recursos)   (balas m))
    , Pictures (map (dibujarExplosion recursos) (explosiones m))
    , dibujarHUD m
    ]

-- =====================================================
-- FONDO
-- =====================================================

dibujarFondo :: Recursos -> Picture
dibujarFondo recursos =
  Translate 0 0 $ Scale 3 3 (imgFondo recursos)

-- =====================================================
-- TANQUES + TORRETAS
-- =====================================================

dibujarTanque :: Recursos -> Tanque -> Picture
dibujarTanque recursos t =
  let (x, y) = posicion t
      info = extra t
      angCuerpo = rad2deg (angulo info)
      angCanon  = rad2deg (anguloCannon info)
      esc = 0.5

      -- Cuerpo y torreta (rotan de forma independiente)
      cuerpo  = Rotate (-angCuerpo) (Scale esc esc (imgTanque recursos))
      torreta = Rotate (-angCanon)  (Scale esc esc (imgTorreta recursos))

      -- Barra de vida
      maxVida = 100
      v       = max 0 (min (fromIntegral maxVida) (energia info))
      ratio   = v / fromIntegral maxVida
      barW    = 40
      barH    = 5
      barY    = y + 30
      baseBar = Translate x barY $ color (greyN 0.25) (rectangleSolid barW barH)
      vidaBar = Translate (x - barW/2 + (barW * ratio)/2) barY $
                color (mixColors ratio (1 - ratio) green red) $
                rectangleSolid (barW * ratio) barH

      -- Nombre del tanque
      nombreTanque = Translate (x - 20) (barY + 10) $
                     Scale 0.1 0.1 $
                     color white $
                     Text (Entidades.nombre (extra t))

  in Pictures
       [ Translate x y (Pictures [cuerpo, torreta])
       , baseBar
       , vidaBar
       , nombreTanque
       ]


-- =====================================================
-- BALAS
-- =====================================================

dibujarBala :: Recursos -> Bala -> Picture
dibujarBala recursos b =
  let (x, y) = posicion b
  in Translate x y
     $ Scale 0.5 0.5
     $ imgBala recursos

-- =====================================================
-- EXPLOSIONES
-- =====================================================

dibujarExplosion :: Recursos -> Explosion -> Picture
dibujarExplosion recursos e =
  let (x, y) = posicion e
      s = sin ((tiempoVida (extra e) / duracionExplosiones) * pi)
  in Translate x y
     $ Scale s s
     $ imgExplosion recursos

-- =====================================================
-- HUD (texto de robots vivos)
-- =====================================================

dibujarHUD :: Mundo -> Picture
dibujarHUD m =
  let vivos = countActiveRobots (tanques m)
  in Translate (-fromIntegral (700 `div` 2) + 20)
               (fromIntegral (700 `div` 2) - 20)
     $ Scale 0.12 0.12
     $ color white
     $ Text ("Naves vivas: " ++ show vivos)
