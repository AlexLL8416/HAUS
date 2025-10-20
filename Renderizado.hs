-- Renderizado.hs
-- Dibuja el estado del juego:
--  - Tanques y Cañón como rectángulos
--  - Balas como círculos
--  - Explosiones (estilo libre): círculos concéntricos
--  - Barra de vida como rectángulos

module Renderizado (dibujarMundo) where

import Graphics.Gloss
import Entidades
import Logica (countActiveRobots)
import Fisicas (rad2deg)

-- Paleta básica
azulTanque   :: Color
azulTanque   = makeColorI 30 144 255 255  -- dodger blue
naranjaCanon :: Color
naranjaCanon = makeColorI 255 140 0 255
amarilloBala :: Color
amarilloBala = makeColorI 255 255 0 255

dibujarMundo :: Mundo -> Picture
dibujarMundo m =
  Pictures
    [ Pictures (map dibujarTanque (tanques m))
    , Pictures (map dibujarBala   (balas m))
    , Pictures (map dibujarExplosion (explosiones m))
    , dibujarHUD m
    ]

-- ========== TANQUES ==========
dibujarTanque :: Tanque -> Picture
dibujarTanque t =
  let (x, y)   = posicion t
      (w, h)   = tamanyo t
      info     = extra t
      angDeg   = rad2deg (anguloCannon info)
      -- cuerpo
      cuerpo   = color azulTanque (rectangleSolid w h)
      borde    = color (withAlpha 0.4 white) (rectangleWire w h)
      -- cañón: barra fina que sale del centro y rota según el ángulo
      canL     = longitudCannon info
      canG     = max 4 (h * 0.18)
      canon    = color naranjaCanon
               $ Rotate (-angDeg)
               $ Translate (canL/2) 0
               $ rectangleSolid canL canG
      -- barra vida
      maxVida  = 100
      v        = max 0 (min (fromIntegral maxVida) (energia info))
      ratio    = v / fromIntegral maxVida
      barW     = w
      barH     = max 4 (h * 0.18)
      barY     = y + h/2 + barH + 4
      baseBar  = Translate x barY $ color (greyN 0.25) (rectangleSolid barW barH)
      vidaBar  = Translate (x - barW/2 + (barW*ratio)/2) barY
               $ color (mixColors ratio (1 - ratio) green red)
               $ rectangleSolid (barW*ratio) barH
      nombre   = Translate (x - 20) (barY + 10) $ color white $ Scale 0.1 0.1 $ Text (Entidades.nombre (extra t))
  in Pictures
      [ Translate x y (Rotate (-rad2deg (angulo info)) (Pictures [cuerpo, borde]))
      , Translate x y canon
      , baseBar, vidaBar, nombre
      ]

-- ========== BALAS ==========
dibujarBala :: Bala -> Picture
dibujarBala b =
  let (x, y) = posicion b
      (w, h) = tamanyo b
      r      = max 2 (min 12 ((w + h) / 4))
  in Translate x y $ color amarilloBala (circleSolid r)

-- ========== EXPLOSIONES ==========
dibujarExplosion :: Explosion -> Picture
dibujarExplosion e =
  let (x, y) = posicion e
      InfoExplosion { radio = r, tiempoVida = t } = extra e
      alpha1 = max 0 (0.7 * t)      -- transparencia decrece con el tiempo
      alpha2 = max 0 (0.4 * t)
      alpha3 = max 0 (0.2 * t)
      c1 = color (withAlpha alpha1 (makeColorI 255 69 0 255))   $ thickCircle (r*0.3) (r*0.3)
      c2 = color (withAlpha alpha2 (makeColorI 255 215 0 255))  $ thickCircle (r*0.6) (r*0.2)
      c3 = color (withAlpha alpha3 (makeColorI 255 255 255 255))$ thickCircle (r*0.9) (r*0.1)
  in Translate x y (Pictures [c1, c2, c3])


-- ========== HUD ==========
dibujarHUD :: Mundo -> Picture
dibujarHUD m =
  let vivos = countActiveRobots (tanques m)
  in Translate (-fromIntegral  (700 `div` 2) + 20) (fromIntegral (700 `div` 2) - 20)
     $ Scale 0.12 0.12
     $ color white
     $ Text ("Robots vivos: " ++ show vivos)
