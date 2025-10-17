import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Estado = Estado {colorFondo :: Color, esCirculo :: Bool, esRectangulo :: Bool, esTriangulo :: Bool}

estadoInicial :: Estado
estadoInicial = Estado {colorFondo = white, esCirculo = True, esRectangulo = False, esTriangulo = False}

dibujar :: Estado -> Picture
dibujar e = Pictures [Color fondo $ rectangleSolid 2000 2000
                        , Translate 0 0 $
                        if circulo then Color blue $ circleSolid 30
                        else if rectangulo then Color yellow $ rectangleSolid 30 40
                        else Rotate (-90) $ Translate 0 20 $ Polygon [(30, 0), (-30, 0), (-30, -30)]
                        , Color black $ Translate (-325) 150 $ Scale 0.1 0.1
                        $ Text "Presionar tecla 'c' para cambiar color del fondo. Presionar tecla 'f' para cambiar la forma de la figura"]
    where
        fondo = colorFondo e
        circulo = esCirculo e
        rectangulo = esRectangulo e

manejarEvento :: Event -> Estado -> Estado
manejarEvento ev es =
    case ev of
        EventKey (Char 'c') Down _ _ -> es {colorFondo = siguienteColor fondo}
        EventKey (Char 'f') Down _ _ -> cambiaEstado es
        _ -> es
    where
        fondo = colorFondo es
        circulo = esCirculo es
        rectangulo = esRectangulo es

siguienteColor :: Color -> Color
siguienteColor c
    | c == white = makeColor 0.83 1.95 1.89 1
    | c == makeColor 0.83 1.95 1.89 1 = makeColor 1 0 1 1
    | otherwise = white

cambiaEstado :: Estado -> Estado
cambiaEstado e
    | circulo = e {esCirculo = False, esRectangulo = True, esTriangulo = False}
    | rectangulo = e {esCirculo = False, esRectangulo = False, esTriangulo = True}
    | otherwise = e {esCirculo = True, esRectangulo = False, esTriangulo = False}
    where
        circulo =  esCirculo e
        rectangulo = esRectangulo e

main :: IO ()
main = play ventana white 60 estadoInicial dibujar manejarEvento actualizar
    where
        ventana = InWindow "Ejemplo 3" (800, 800) (10, 10)
        actualizar _ estado = estado
