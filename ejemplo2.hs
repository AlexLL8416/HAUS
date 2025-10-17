import Graphics.Gloss

main :: IO ()
main = animate ventana fondo dibujo
    where
        ventana = InWindow "Ejemplo 2" (600, 600) (10, 10)
        fondo = white
        dibujo tiempo = Pictures[ Rotate (tiempo * 100) (Color red $ rectangleSolid 30 30),
            Color blue $ Translate (-50) (50 * sin tiempo) $ circleSolid 25,
            Rotate ((-tiempo) * 70) (Color (makeColor 1 0 1 1) $ Translate 0 70 $ Polygon[(30, 0), (-30, 0), (-30, -30)])] 
            -- Negativo para invertr el sentido de la rotación
