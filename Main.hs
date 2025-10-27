module Main where

-- Main.hs: configura y ejecuta el juego 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random

-- Importamos tus módulos
import Entidades       
import Logica          
import Colisiones      
import Renderizado     
import DSLAcciones     
import Fisicas         
import Memoria         
import Utilidades      

-- =====================================================
-- CONFIGURACIÓN DE LA VENTANA
-- =====================================================

tamanyoCampo :: Num a => (a, a)
tamanyoCampo = (700, 700)

ventana :: Display
ventana = InWindow "Torneo de Bots" tamanyoCampo (100, 100)

colorFondo :: Color
colorFondo = black

fps :: Int
fps = 60

-- =====================================================
-- INICIALIZACIÓN DEL MUNDO
-- =====================================================

inicializarMundo :: IO Mundo
inicializarMundo = do
    x1 <- randomRIO (-300, 300)
    y1 <- randomRIO (-300, 300)
    x2 <- randomRIO (-300, 300)
    y2 <- randomRIO (-300, 300)
    x3 <- randomRIO (-300, 300)
    y3 <- randomRIO (-300, 300)
    x4 <- randomRIO (-300, 300)
    y4 <- randomRIO (-300, 300)
    x5 <- randomRIO (-300, 300)
    y5 <- randomRIO (-300, 300)
    x6 <- randomRIO (-300, 300)
    y6 <- randomRIO (-300, 300)

    let mundo =
            addTanque (x1,y1) (\t -> t { cerebro = botSimple })
          $ addTanque (x2,y2) (\t -> t { cerebro = botFrancotirador })
          $ addTanque (x3,y3) (\t -> t { cerebro = botEvasivo })
          $ addTanque (x4,y4) (\t -> t { cerebro = botVueltas })
          $ addTanque (x5,y5) (\t -> t { cerebro = botRencoroso })   
          $ addTanque (x6,y6) (\t -> t { cerebro = botCobarde })     
              (mundoVacio tamanyoCampo)
    return mundo

-- =====================================================
-- RENDERIZADO, EVENTOS Y ACTUALIZACIÓN
-- =====================================================

renderizar :: Recursos -> (Mundo, Recursos) -> Picture
renderizar recursos (mundo, _) = dibujarMundo recursos mundo

manejarEvento :: Event -> (Mundo, Recursos) -> (Mundo, Recursos)
manejarEvento (EventKey (MouseButton LeftButton) Down _ (x, y)) (m, r) =
    (addTanque (x, y) (\t -> t { cerebro = botSimple }) m, r)
manejarEvento _ s = s

actualizar :: Float -> (Mundo, Recursos) -> (Mundo, Recursos)
actualizar dt (m, r) = (updateMundo dt m, r)

-- =====================================================
-- FUNCIÓN PRINCIPAL
-- =====================================================

main :: IO ()
main = do
    putStrLn "Iniciando Torneo de Bots..."

    -- Carga de imágenes (usa Juicy)
    Just tanquePic    <- loadJuicyPNG "assets/tanque.png"
    Just torretaPic   <- loadJuicyPNG "assets/torreta.png"
    Just balaPic      <- loadJuicyPNG "assets/bala.png"
    Just explosionPic <- loadJuicyPNG "assets/explosion.png"
    Just fondoPic     <- loadJuicyPNG "assets/fondo.png"

    let recursos = Recursos
          { imgTanque     = tanquePic
          , imgTorreta    = torretaPic
          , imgBala       = balaPic
          , imgExplosion  = explosionPic
          , imgFondo      = fondoPic
          }

    mundoInicial <- inicializarMundo

    play
      ventana
      colorFondo
      fps
      (mundoInicial, recursos)
      (renderizar recursos)
      manejarEvento
      actualizar
