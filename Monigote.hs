-- Importa las funciones básicas de la librería Gloss para dibujar y gestionar eventos
import Graphics.Gloss
-- Importa la interfaz de juego pura (play, Event, SpecialKey, etc.)
import Graphics.Gloss.Interface.Pure.Game

-- ======================
-- CONFIGURACIÓN GENERAL
-- ======================

-- Declara el ancho y alto de la ventana como enteros
ancho, alto :: Int
ancho = 800  -- ancho de la ventana en píxeles
alto  = 600  -- alto de la ventana en píxeles

-- Posición vertical (y) del suelo en coordenadas de la escena
pisoY :: Float
pisoY = -200  -- valor negativo para colocar el suelo por debajo del centro

-- Límite horizontal máximo que puede alcanzar el personaje (margen para que no salga)
limiteX :: Float
limiteX = fromIntegral ancho / 2 - 30  -- mitad del ancho menos margen, convierte ancho a Float y opera

-- Parámetros físicos del juego (valores en unidades "píxel/seg" y "píxel/seg^2")
gravedad, velSalto, velCaminar :: Float
gravedad   = -900   -- aceleración por gravedad (pix/s^2)
velSalto   = 400    -- velocidad inicial vertical al saltar (pix/s)
velCaminar = 200    -- velocidad de desplazamiento horizontal (pix/s)

-- ======================
-- ESTADO DEL JUEGO
-- ======================

-- Tipo que representa todo el estado necesario del juego
data Estado = Estado
  { posX   :: Float  -- posición horizontal actual del monigote
  , posY   :: Float  -- posición vertical actual del monigote
  , velY   :: Float  -- velocidad vertical (para salto/caída)
  , dir    :: Float  -- dirección horizontal: 1 = derecha, -1 = izquierda
  , suelo  :: Bool   -- True si el personaje está tocando el suelo
  , izq    :: Bool   -- True si la flecha izquierda está presionada
  , der    :: Bool   -- True si la flecha derecha está presionada
  , tiempo :: Float  -- acumulador de tiempo para controlar la animación de las piernas
  } deriving Show  -- deriving Show para facilitar depuración

-- Estado inicial: monigote centrado, en el suelo, sin movimiento, mirando a la derecha
estadoInicial :: Estado
estadoInicial = Estado 0 pisoY 0 1 True False False 0

-- ======================
-- FUNCIÓN PRINCIPAL (entry point)
-- ======================

-- main arranca la ventana y el bucle de juego con play
main :: IO ()
main = play ventana fondo fps estadoInicial dibuja manejarEvento actualizar
  where
    ventana = InWindow "Monigote - Andar con Rotación (perfil)" (ancho, alto) (100, 100)
    fondo   = white  -- color de fondo de la ventana
    fps     = 60     -- frames por segundo para la actualización

-- ======================
-- DIBUJADO: dibuja la escena completa por frame
-- ======================

-- dibuja recibe el estado actual y devuelve la imagen (Picture) que se muestra en pantalla
dibuja :: Estado -> Picture
dibuja e = Pictures
  [ -- HUD: texto en la esquina superior izquierda con instrucciones y estado
    Translate (-350) 250 $                       -- mueve el texto a la esquina superior izquierda
      Scale 0.12 0.12 $                          -- escala el tamaño del texto
      Color black $                              -- color del texto
      Text ("←/→: Mover | ↑: Saltar\n"            -- línea 1: instrucciones
            ++ "X: " ++ show (round $ posX e)    -- línea 2: muestra la posición X (redondeada)
            ++ " | " ++ (if suelo e then "Suelo" else "Aire")), -- estado: suelo/aire

    -- Suelo: rectángulo ancho y plano de color gris
    Color (greyN 0.5) $                          -- color gris medio
      Translate 0 (pisoY - 50) $                 -- posiciona el rectángulo en la y del suelo
      rectangleSolid (fromIntegral ancho) 30,    -- ancho = ancho ventana, alto fijo 30

    -- Monigote: cabeza, cuerpo y piernas (posicionado en posX,posY)
    Translate (posX e) (posY e) $                -- traslada el monigote a su posición
      Scale (dir e) 1 $                          -- voltea horizontalmente si dir = -1
      Pictures [cabeza, cuerpo, piernas (tiempo e) (estaMoviendo e) (suelo e)] -- componentes
  ]

  where
    -- Cabeza: círculo color piel desplazado hacia arriba respecto al centro del cuerpo
    cabeza = Translate 0 40 $  -- colocar cabeza sobre el cuerpo
      Pictures
        [ -- Cabeza (base piel)
          Color (makeColorI 255 220 180 255) $
            circleSolid 15,

          -- Ojos blancos
          Translate 5 3 $ Color white $ circleSolid 4,   -- ojo derecho
          Translate (-5) 3 $ Color white $ circleSolid 4, -- ojo izquierdo

          -- Pupilas (negras, un poco desplazadas hacia donde mira)
          Translate 6 3 $ Color black $ circleSolid 2,
          Translate (-6) 3 $ Color black $ circleSolid 2,

          -- Boca: una línea corta o sonrisa
          Color black $
            Translate 3 (-8) $  -- posición debajo de los ojos
            Scale 0.8 0.8 $
            Line [(-5,0),(5,0)]  -- línea simple como boca
        ]


    -- Cuerpo: rectángulo rojo centrado ligeramente hacia arriba
    cuerpo = Color red $
             Translate 0 10 $
             rectangleSolid 20 40                   -- ancho 20, alto 40

    -- Piernas: función que genera las dos piernas, rotando desde la cadera (hip)
    piernas :: Float -> Bool -> Bool -> Picture
    piernas t mov enSuelo
      -- Si se mueve horizontalmente y está en el suelo, animamos rotando desde la cadera
      | mov && enSuelo =
          let
            ang   = 30 * sin (t * 8)   -- ángulo en grados: amplitud 30°, frecuencia ajustable
            hipY  = -10               -- coordenada Y de la cadera (parte inferior del cuerpo)
            hipX  = 5                 -- distancia horizontal desde el centro del cuerpo a la cadera
            -- Pierna izquierda: trasladar a la cadera izquierda, rotar y dibujar desde la cadera hacia abajo
            piernaIzq = Translate (-hipX) hipY $ Rotate ang  $ Color blue $ rectFromHip 6 30
            -- Pierna derecha: trasladar a la cadera derecha, rotar en sentido contrario y dibujar
            piernaDer = Translate hipX       hipY $ Rotate (-ang) $ Color blue $ rectFromHip 6 30
          in Pictures [piernaIzq, piernaDer]  -- devuelve ambas piernas como una sola imagen
      -- Si está quieto o en el aire, piernas rectas (no rotadas)
      | otherwise =
          let hipY = -10
              hipX = 5
              piernaIzq = Translate (-hipX) hipY $ Color blue $ rectFromHip 6 30
              piernaDer = Translate hipX       hipY $ Color blue $ rectFromHip 6 30
          in Pictures [piernaIzq, piernaDer]

-- ======================
-- FUNCIÓN AUXILIAR PARA LAS PIERNAS
-- ======================

-- rectFromHip crea un rectángulo cuya parte superior (hip) queda en el origen,
-- de modo que al rotarlo lo hace alrededor de la cadera (perfecto para piernas).
rectFromHip :: Float -> Float -> Picture
rectFromHip w h =
  Translate 0 (-h / 2) $   -- desplaza el centro del rectángulo hacia abajo para que el borde superior quede en y=0
  rectangleSolid w h       -- rectángulo centrado originalmente en el origen

-- ======================
-- AUXILIAR: indica si el personaje está moviéndose horizontalmente
-- ======================

-- Devuelve True si exactamente una de las teclas izquierda/derecha está presionada
estaMoviendo :: Estado -> Bool
estaMoviendo e = izq e /= der e

-- ======================
-- MANEJO DE EVENTOS (teclado)
-- ======================

-- manejarEvento procesa eventos y devuelve el nuevo estado
manejarEvento :: Event -> Estado -> Estado

-- Pulsar flecha izquierda: marca 'izq' y pone la dirección a -1 (mirando a la izquierda)
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) e =
  e { izq = True, dir = -1 }

-- Pulsar flecha derecha: marca 'der' y pone la dirección a 1 (mirando a la derecha)
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) e =
  e { der = True, dir = 1 }

-- Soltar flecha izquierda: desmarca 'izq'
manejarEvento (EventKey (SpecialKey KeyLeft) Up _ _) e =
  e { izq = False }

-- Soltar flecha derecha: desmarca 'der'
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) e =
  e { der = False }

-- Pulsar flecha arriba: si está en el suelo, aplicar velocidad de salto y marcar que ya no está en suelo
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) e
  | suelo e = e { velY = velSalto, suelo = False }

-- Cualquier otro evento no cambia el estado
manejarEvento _ e = e

-- ======================
-- ACTUALIZACIÓN POR FRAME (física y animación)
-- ======================

-- actualizar recibe el tiempo delta (segundos) y el estado actual, devuelve el estado nuevo
actualizar :: Float -> Estado -> Estado
actualizar dt e =
  let
    -- Desplazamiento horizontal según teclas mantenidas (velocidad constante * dt)
    dx
      | izq e && not (der e) = -velCaminar * dt  -- si solo izq está presionada, moverse a la izquierda
      | der e && not (izq e) =  velCaminar * dt  -- si solo der está presionada, moverse a la derecha
      | otherwise             = 0                -- si ninguna o ambas, no moverse horizontalmente

    -- Calcula la nueva posición X y la limita dentro de [-limiteX, limiteX]
    nuevoX = max (-limiteX) $ min limiteX $ posX e + dx

    -- Física vertical: actualiza velocidad por gravedad y posición por velocidad
    vy'     = velY e + gravedad * dt           -- nueva velocidad vertical integrando la gravedad
    y'      = posY e + vy' * dt                -- nueva posición vertical integrando la velocidad
    enSuelo = y' <= pisoY                      -- si la nueva y está por debajo del suelo, consideramos que toca suelo
    yFinal  = if enSuelo then pisoY else y'     -- si toca suelo, fijamos la posición en pisoY
    vyFinal = if enSuelo then 0 else vy'        -- si toca suelo, velocidad vertical = 0

    -- Actualiza el contador de tiempo para la animación solo si el personaje se está moviendo
    tiempo' = if estaMoviendo e then tiempo e + dt else tiempo e
  in
    -- Devuelve el nuevo estado con todos los campos actualizados
    e { posX = nuevoX
      , posY = yFinal
      , velY = vyFinal
      , suelo = enSuelo
      , tiempo = tiempo'
      }
