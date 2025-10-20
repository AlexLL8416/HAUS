module Main where

-- Importamos la librería Gloss en modo "Pure Game" (no IO)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Importamos los módulos del proyecto
import Entidades       
import Logica          
import Colisiones      
import Renderizado     
import DSLAcciones     
import Fisicas         
import Memoria         
import Utilidades      


-- CONFIGURACIÓN DE LA VENTANA --

tamanyoCampo :: Num a => (a, a)
tamanyoCampo = (700, 700)

-- Definimos la ventana de juego
ventana :: Display
ventana = InWindow "Torneo de Bots" tamanyoCampo (100, 100)

-- Color de fondo de la ventana
colorFondo :: Color
colorFondo = black

-- Frecuencia de actualización en FPS
fps :: Int
fps = 60


-- INICIALIZACIÓN DEL MUNDO --

-- Inicializa el estado del juego (posición de tanques, balas, etc.)
inicializarMundo :: Mundo
inicializarMundo =
      addTanque ( 250,0) (\t -> t { cerebro = botSimple })
    $ addTanque (-250,0) (\t -> t { cerebro = botFrancotirador })
    $ addTanque (-100,100) (\t -> t { cerebro = botEvasivo })
    $ addTanque (0, -250) (\t -> t { cerebro = botVueltas })
    (mundoVacio tamanyoCampo)


-- RENDERIZADO DEL MUNDO --

-- Dibuja el estado actual del juego en pantalla.
-- Esta función llamará a las rutinas de Renderizado.hs para dibujar tanques, proyectiles, explosiones, barras de vida, etc.
renderizar :: Mundo -> Picture
renderizar mundo = 
    dibujarMundo mundo


-- MANEJO DE EVENTOS (teclado, ratón, etc.) --

-- Procesa los eventos de entrada del usuario. Por ahora al hacer click genera un tanque de tipo Simple.
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (MouseButton LeftButton) Down _ (x, y)) m = addTanque (x, y) (\t -> t { cerebro = botSimple }) m
manejarEvento _ mundo = mundo


-- ACTUALIZACIÓN DEL MUNDO (BUCLE PRINCIPAL) --

-- Esta función se llama en cada frame para avanzar la simulación del juego.
actualizar :: Float -> Mundo -> Mundo
actualizar = updateMundo


-- FUNCIÓN PRINCIPAL --

main :: IO ()
main = do
    putStrLn "Iniciando Torneo de Bots..."
    play
        ventana           -- Configuración de la ventana
        colorFondo        -- Color de fondo
        fps               -- Fotogramas por segundo
        inicializarMundo  -- Estado inicial del juego
        renderizar        -- Función para dibujar el mundo
        manejarEvento     -- Manejo de eventos (teclado, ratón)
        actualizar        -- Función de actualización por frame
